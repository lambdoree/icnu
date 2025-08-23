(define-module (icnu icnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (icnu utils format)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils log)
  #:export (
	    ;; data builders (surface s-expr)
	    mk-node mk-wire mk-par mk-nu
		    ;; parse/print
		    parse-net pretty-print
		    ;; execution (DEPRECATED - MOVED TO cpnet/runtime)
		    ;; utilities
		    empty-net copy-net make-fresh-name all-names node-agent endpoint valid-port?
		    peer net-nodes net-links get-ports unlink-port!
		    rewire! delete-node! all-nodes-with-agent find-active-pairs
		    ;; runtime hooks
		    set-link-conflict-mode!
		    *link-conflict-mode*
		    mark-nu!
		    link-peers!
		    add-node!
		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0. Internal representation
;;
;; Node types: 'A 'C 'E (core), and 'V (internal stub for free names)
;; Ports: 'p 'l 'r
;;
;; Net:
;;  - nodes : hash-table name => agent
;;  - links : hash-table (name . port) => (name' . port')  (undirected; mirrored)
;;  - nu    : hash-table name => #t    (flat ν binder set for printing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <net>
  (make-net nodes links nu counter)
  net?
  (nodes net-nodes)  ;; hash name -> agent
  (links net-links)  ;; hash (cons name port) -> (cons name port)
  (nu    net-nu)     ;; hash name -> #t
  (counter net-counter set-net-counter!)) ;; integer counter for fresh-name generation

(define (empty-net)
  (make-net (make-hash-table) (make-hash-table) (make-hash-table) 0))

(define (copy-net n)
  "Deep-copy internal net tables to produce an independent net instance.
   This implementation avoids parsing back into surface forms and instead
   clones the underlying hash-tables and recreates fresh cons keys/values
   for link entries so the returned net does not share mutable structures
   with the source."
  (let ((nn (make-hash-table))
        (ll (make-hash-table))
        (nu (make-hash-table)))
    ;; copy nodes (name -> agent)
    (hash-for-each (lambda (k v) (hash-set! nn k v)) (net-nodes n))
    ;; copy links: create new cons keys/values for each link entry so that
    ;; the copied net has independent pair objects (keys) and values.
    (hash-for-each
     (lambda (k v)
       (let ((k2 (if (pair? k) (cons (car k) (cdr k)) k))
             (v2 (if (pair? v) (cons (car v) (cdr v)) v)))
         (hash-set! ll k2 v2)))
     (net-links n))
    ;; copy nu bindings
    (hash-for-each (lambda (k v) (hash-set! nu k v)) (net-nu n))
    ;; preserve counter
    (make-net nn ll nu (net-counter n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Basic ops: nodes/links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (valid-port? p) (memq p '(p l r)))

(define (get-ports agent-type)
  (case agent-type
    ((A C) '(p l r))
    ((E)   '(p))
    (else '())))

(define (ensure-node! n name agent)
  (let* ((tbl (net-nodes n))
         (existing-agent (hash-ref tbl name #f)))
    (cond
     ((not existing-agent)
      (hash-set! tbl name agent))
     ((eq? existing-agent 'V)
      (hash-set! tbl name agent))
     ((not (eq? existing-agent agent))
      (error "Node already exists with different agent" name agent existing-agent)))))

(define (add-node! n name agent)
  (when (not (memq agent '(A C E V)))
    (error "Unknown agent" agent))
  (ensure-node! n name agent)
  n)

(define (node-agent n name)
  (hash-ref (net-nodes n) name #f))

(define (endpoint name port)
  (when (not (valid-port? port)) (error "Invalid port" port))
  (cons name port))

(define *link-peers-warned* (make-hash-table))

;; Link conflict policy:
;; - *link-conflict-mode* (parameter) controls behavior when a-port or b-port is already linked.
;;   'error               -> raise an error (default)
;;   'warn                -> warn once per pair and skip creating the conflicting link (legacy)
;;   'overwrite-injection -> prefer injection/temporary ports and overwrite previous mappings
(define *link-conflict-mode* (make-parameter 'error))

(define (set-link-conflict-mode! v)
  (cond
   ((boolean? v) (set-link-conflict-mode! (if v 'warn 'error)))
   ((symbol? v) (*link-conflict-mode* v))
   (else (error "set-link-conflict-mode!: invalid arg" v))))

(define (link-peers! n a-port b-port)
  ;; undirected link (idempotent & tolerant):
  ;; - If the exact a-port <-> b-port mapping already exists, do nothing.
  ;; - If either port is linked to a different peer, handle according to *link-conflict-mode*.
  (let ((L (net-links n)))
    (let ((exist-a (peer n a-port))
          (exist-b (peer n b-port)))
      (cond
       ;; already linked the same way: idempotent no-op
       ((and exist-a (equal? exist-a b-port)) #t)
       ((and exist-b (equal? exist-b a-port)) #t)
       ;; port linked to a different peer -> handle per policy
       ((or exist-a exist-b)
        (let* ((mode (*link-conflict-mode*))
               (a-name-symbol (and (pair? a-port) (symbol? (car a-port)) (car a-port)))
               (b-name-symbol (and (pair? b-port) (symbol? (car b-port)) (car b-port)))
               (a-name (if a-name-symbol (symbol->string a-name-symbol) (format-string #f "~a" a-port)))
               (b-name (if b-name-symbol (symbol->string b-name-symbol) (format-string #f "~a" b-port)))
               (key (format-string #f "~a<->~a" a-name b-name)))
          (cond
           ((eq? mode 'error)
            (error "link-peers!: conflicting link between" a-port b-port))
           ((eq? mode 'overwrite-injection)
            ;; When in overwrite-injection mode, always prefer the new link by
            ;; removing any existing connections on the affected ports before
            ;; creating the new one. This is robust because unlink-port! finds
            ;; keys by value (equal?) rather than identity (eq?).
            (when exist-a (unlink-port! n a-port))
            (when exist-b (unlink-port! n b-port))
            (hash-set! L a-port b-port)
            (hash-set! L b-port a-port)
            #t)
           ((eq? mode 'warn)
            ;; Legacy behavior: log at debug level and skip creating the conflicting link.
            ;; Preserve backward-compatibility with existing tests that expect
            ;; 'warn' to not raise an exception; emit a single debug message per pair
            ;; and skip creating the conflicting mapping so test runs are not polluted.
            (unless (hash-ref *link-peers-warned* key #f)
              (hash-set! *link-peers-warned* key #t)
              (debugf 2 "link-peers!: skipping conflicting link between ~a and ~a; existing peer present.\n" a-name b-name))
            #t)
           (else
            ;; Unknown conflict mode -> fail fast rather than silently skipping.
            (error "link-peers!: unknown conflict-mode" mode a-port b-port)))))
       (else
        (hash-set! L a-port b-port)
        (hash-set! L b-port a-port)
	n)))))

(define (unlink-port! n a-port)
  (let* ((L (net-links n))
         (b-port (peer n a-port)))
    (when b-port
      ;; We found a link. Now we must remove BOTH directions.
      ;; b-port is the actual value from the hash, so it's a valid key
      ;; for the reverse link.
      (hash-remove! L b-port)
      ;; For a-port, it might be a fresh cons. We have to find the key
      ;; that is equal to it.
      (let ((key-to-remove #f))
        (hash-for-each (lambda (k v)
                         (when (equal? k a-port)
                           (set! key-to-remove k)))
		       L)
        (when key-to-remove
          (hash-remove! L key-to-remove)))))
  n)

(define (peer n a-port)
  ;; Try direct hash lookup first (fast path). Some callers construct fresh
  ;; cons cells like (cons name 'p) which are not `eq?` to the cons used as
  ;; the original hash key; in that case fall back to scanning the links table
  ;; for a matching (name . port) pair by value.
  (let ((direct (hash-ref (net-links n) a-port #f)))
    (if direct
        direct
        (let ((found #f))
          (hash-for-each
           (lambda (k v)
             (when (and (eq? (car k) (car a-port))
                        (eq? (cdr k) (cdr a-port)))
	       (set! found v)))
           (net-links n))
          found))))

(define (rewire! n from to)
  (unless (and (pair? from) (pair? to))
    (error "rewire!: endpoints must be pair names" from to))
  (unlink-port! n from)
  (unlink-port! n to)
  (when (or (peer n from) (peer n to))
    (error "rewire!: attempted to link ports that are still connected" from to))
  (link-peers! n from to))

(define (delete-node! n x)
  (let ((agent (node-agent n x)))
    (when agent
      (for-each (lambda (pt) (unlink-port! n (cons x pt))) (get-ports agent))
      (hash-remove! (net-nodes n) x)))
  n)

(define (all-nodes-with-agent net agent-type)
  (let ((acc '()))
    (hash-for-each
     (lambda (name agent)
       (when (eq? agent agent-type)
         (set! acc (cons name acc))))
     (net-nodes net))
    (reverse acc)))

(define (find-active-pairs net)
  (let* ((nodes (net-nodes net))
         (L     (net-links net))
         (pairs '())
         (seen  (make-hash-table)))
    (hash-for-each
     (lambda (ep peer-ep)
       (match ep
         (((? symbol? a) . 'p)
          (match peer-ep
            (((? symbol? b) . 'p)
             (let* ((A (hash-ref nodes a #f))
                    (B (hash-ref nodes b #f)))
               (when (and A B (memq A '(A C E)) (memq B '(A C E)))
                 (let* ((ka (symbol->string a))
                        (kb (symbol->string b))
                        (key (if (string<? ka kb) (cons a b) (cons b a))))
                   (unless (hash-ref seen key #f)
                     (hash-set! seen key #t)
                     (set! pairs (cons (list (cons a A) (cons b B)) pairs)))))))
            (_ #f)))
         (_ #f)))
     L)
    pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Fresh names (ν)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (all-names n)
  (let ((s '()))
    (hash-for-each (lambda (k v) (set! s (cons k s))) (net-nodes n))
    s))

(define (make-fresh-name n . maybe-prefix)
  ;; Use a per-net counter to avoid O(N) scans of all names on each call.
  (let ((prefix (if (null? maybe-prefix) "n" (car maybe-prefix))))
    (letrec ((loop (lambda ()
                     (let* ((i (net-counter n))
                            (cand (string->symbol (format-string #f "~a-~a" prefix i))))
		       (if (hash-ref (net-nodes n) cand #f)
                           (begin (set-net-counter! n (+ i 1)) (loop))
                           (begin (set-net-counter! n (+ i 1)) cand))))))
      (loop))))

(define (mark-nu! n name) (hash-set! (net-nu n) name #t) n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Parsing surface s-expr -> net
;;
;; Surface forms:
;;  (node a A)         ; a:A
;;  (wire (a p) (b r)) ; a.p ~ b.r
;;  (par e1 e2 ...)    ; e1 | e2 | ...
;;  (nu (a b ...) body)
;;
;; Free names on a wire endpoint implicitly materialize as V nodes (internal).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `(node ...)` S-expression을 생성하는 헬퍼 함수입니다.
(define (mk-node name agent) `(node ,name ,agent))
;; `(wire ...)` S-expression을 생성하는 헬퍼 함수입니다.
(define (mk-wire a p b q) `(wire (,a ,p) (,b ,q)))
;; `(par ...)` S-expression을 생성하는 헬퍼 함수입니다.
(define (mk-par . elts) `(par ,@elts))
;; `(nu ...)` S-expression을 생성하는 헬퍼 함수입니다.
(define (mk-nu names body) `(nu ,names ,body))

(define (parse-endpoint n ep)
  (match ep
    (('quote ((? symbol? a) (? symbol? p)))
     (parse-endpoint n (list a p)))
    (((? symbol? a) (? symbol? p))
     (unless (valid-port? p) (error "parse: invalid port" p))
     (unless (node-agent n a)
       (let* ((s (symbol->string a))
	      (starts-with?
	       (lambda (str pref)
                 (let ((ls (string-length str)) (lp (string-length pref)))
                   (and (>= ls lp) (string=? (substring str 0 lp) pref)))))
	      (is-literal
	       (or (starts-with? s "inj-")
                   (starts-with? s "lit-")
                   (starts-with? s "num-")
                   (starts-with? s "church-")
                   (starts-with? s "cons-")
                   (starts-with? s "nil-")
                   (starts-with? s "app-")
                   (string=? s "true")
                   (string=? s "false")))
	      (is-qualified (string-contains? s ".")))
         (if (or is-literal is-qualified)
             (add-node! n a 'A)
             (add-node! n a 'V))))
     (endpoint a p))

    (('list ('quote (? symbol? a)) ('quote (? symbol? p)))
     (parse-endpoint n (list a p)))
    (('list ('quote (? symbol? a)) (? symbol? p))
     (unless (valid-port? p) (error "parse: invalid port" p))
     (unless (node-agent n a)
       (let* ((s (symbol->string a))
	      (starts-with?
	       (lambda (str pref)
                 (let ((ls (string-length str)) (lp (string-length pref)))
                   (and (>= ls lp) (string=? (substring str 0 lp) pref)))))
	      (is-literal
	       (or (starts-with? s "inj-")
                   (starts-with? s "lit-")
                   (starts-with? s "num-")
                   (starts-with? s "church-")
                   (starts-with? s "cons-")
                   (starts-with? s "nil-")
                   (starts-with? s "app-")
                   (string=? s "true")
                   (string=? s "false")))
	      (is-qualified (string-contains? s ".")))
         (if (or is-literal is-qualified)
             (add-node! n a 'A)
             (add-node! n a 'V))))
     (endpoint a p))
    (('list (? symbol? a) ('quote (? symbol? p)))
     (parse-endpoint n (list a p)))
    (('list (? symbol? a) (? symbol? p))
     (unless (valid-port? p) (error "parse: invalid port" p))
     (unless (node-agent n a)
       (let* ((s (symbol->string a))
	      (starts-with?
	       (lambda (str pref)
                 (let ((ls (string-length str)) (lp (string-length pref)))
                   (and (>= ls lp) (string=? (substring str 0 lp) pref)))))
	      (is-literal
	       (or (starts-with? s "inj-")
                   (starts-with? s "lit-")
                   (starts-with? s "num-")
                   (starts-with? s "church-")
                   (starts-with? s "cons-")
                   (starts-with? s "nil-")
                   (starts-with? s "app-")
                   (string=? s "true")
                   (string=? s "false")))
	      (is-qualified (string-contains? s ".")))
         (if (or is-literal is-qualified)
             (add-node! n a 'A)
             (add-node! n a 'V))))
     (endpoint a p))

    (else (error "parse: bad endpoint" ep))))

(define (parse-1 n form)
  (match form
    ;; surface forms
    (('node (? symbol? a) (? symbol? agent))
     (add-node! n a agent)
     n)
    (('node ('quote (? symbol? a)) ('quote (? symbol? agent)))
     (add-node! n a agent)
     n)
    (('wire ep1 ep2)
     (let ((e1 (parse-endpoint n ep1))
           (e2 (parse-endpoint n ep2)))
       (link-peers! n e1 e2)
       n))
    (('par . es)
     (fold (lambda (form acc) (parse-1 acc form)) n es))
    (('nu (names ...) body)
     ;; add ν names to set and parse body
     (for-each (lambda (nm) (mark-nu! n nm)) names)
     (parse-1 n body))

    ;; Accept the "mk-*" helper forms produced by the IC^ν expansion stage.
    ;; Some pipeline stages emit mk-node/mk-wire/mk-par/mk-nu during macro-expansion;
    ;; treat them as synonyms of the surface forms above so parsing is robust.
    ;; Support all common quoting combinations produced by macro-expansion:
    (('mk-node (? symbol? a) (? symbol? agent))
     (add-node! n a agent)
     n)
    (('mk-node (? symbol? a) ('quote (? symbol? agent)))
     (add-node! n a agent)
     n)
    (('mk-node ('quote (? symbol? a)) (? symbol? agent))
     (add-node! n a agent)
     n)
    (('mk-node ('quote (? symbol? a)) ('quote (? symbol? agent)))
     (add-node! n a agent)
     n)

    ;; Accept mk-wire in several common argument arrangements emitted by
    ;; macro-expansion: (mk-wire a p b q) and variants where ports are quoted.
    (('mk-wire (? symbol? a) (? symbol? p) (? symbol? b) (? symbol? q))
     (link-peers! n (cons a p) (cons b q))
     n)
    (('mk-wire (? symbol? a) ('quote (? symbol? p)) (? symbol? b) (? symbol? q))
     (link-peers! n (cons a p) (cons b q))
     n)
    (('mk-wire (? symbol? a) (? symbol? p) (? symbol? b) ('quote (? symbol? q)))
     (link-peers! n (cons a p) (cons b q))
     n)
    (('mk-wire (? symbol? a) ('quote (? symbol? p)) (? symbol? b) ('quote (? symbol? q)))
     (link-peers! n (cons a p) (cons b q))
     n)

    ;; Accept 3-arg mk-wire forms where one endpoint is split across a symbol+port
    ;; and the other is a full endpoint expression (e.g. (mk-wire cond-copy-0 'l (list if-impl-0 'p))).
    (('mk-wire (? symbol? a) (? symbol? p) ep2)
     (let ((e1 (endpoint a p))
           (e2 (parse-endpoint n ep2)))
       (link-peers! n e1 e2)
       n))
    (('mk-wire (? symbol? a) ('quote (? symbol? p)) ep2)
     (let ((e1 (endpoint a p))
           (e2 (parse-endpoint n ep2)))
       (link-peers! n e1 e2)
       n))
    ;; Symmetric: first endpoint may be a full endpoint expression while second is symbol+port
    (('mk-wire ep1 (? symbol? b) (? symbol? q))
     (let ((e1 (parse-endpoint n ep1))
           (e2 (endpoint b q)))
       (link-peers! n e1 e2)
       n))
    (('mk-wire ep1 ('quote (? symbol? b)) ('quote (? symbol? q)))
     (let ((e1 (parse-endpoint n ep1))
           (e2 (endpoint b q)))
       (link-peers! n e1 e2)
       n))

    ;; Fallback: the 2-arg form (mk-wire ep1 ep2) where endpoints are full
    ;; endpoint expressions; preserve previous behavior.
    (('mk-wire ep1 ep2)
     (let ((e1 (parse-endpoint n ep1))
           (e2 (parse-endpoint n ep2)))
       (link-peers! n e1 e2)
       n))
    (('mk-par . es)
     (fold (lambda (form acc) (parse-1 acc form)) n es))
    ;; _prepare-icnu-for-eval wraps nu-bound name lists as a quoted list: (mk-nu (quote (names ...)) body)
    (('mk-nu ('quote names) body)
     (for-each (lambda (nm) (mark-nu! n nm)) names)
     (parse-1 n body))

    (else (error "parse: unknown form" form))))

(define (parse-net sexpr)
  "Parse a surface sexpr into a net.
     During parsing we temporarily relax the link-conflict-mode to 'warn
     so that higher-level surface generators (stdlib helpers) can build
     composite forms without triggering hard errors for transient link
     conflicts. The global mode is restored after parsing."
  (let ((old-mode (*link-conflict-mode*)))
    (set-link-conflict-mode! 'warn)
    (let ((res (parse-1 (empty-net) sexpr)))
      (set-link-conflict-mode! old-mode)
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Pretty printer with options
;;
;; (pretty-print net '((show-V? . #f) (show-nu? . #f)))
;;
;; - show-V?  : include internal 'V nodes and their links (default #f)
;; - show-nu? : wrap the printed body with a flat (nu (names...) body)
;;
;; Note: This prints a *flat ν binder*. For exact scope, maintain a scope DAG
;; and compute minimal ν-covers when printing. We never omit a fresh name; we
;; may wrap a slightly larger scope.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pp-bool opt k dflt)
  (let ((v (assq-ref opt k)))
    (if (boolean? v) v dflt)))

(define (pretty-print net . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (showV (pp-bool opts 'show-V? #f))
         (showNu (pp-bool opts 'show-nu? #f))
         (nodes (net-nodes net))
         (links (net-links net)))
    (define (visible-node? name)
      (let ((ag (hash-ref nodes name)))
        (or (not (eq? ag 'V)) showV)))
    (define (visible-endpoint? ep)
      (let ((nm (car ep)))
        (visible-node? nm)))
    (define nodes-out
      (let ( (acc '()) )
        (hash-for-each 
         (lambda (nm ag)
           (when (visible-node? nm)
             (set! acc (cons `(node ,nm ,ag) acc))))
	 nodes)
        (reverse acc)))
    (define links-out
      (let ((seen (make-hash-table))
            (acc '()))
        (hash-for-each
         (lambda (a b)
           ;; print each undirected edge once; a < b lexicographically for stability
           (when (and (visible-endpoint? a) (visible-endpoint? b))
             (let* ((ka (symbol->string (car a)))
                    (kb (symbol->string (car b)))
                    (key (if (string<? ka kb) (cons a b) (cons b a))))
               (unless (hash-ref seen key #f)
                 (hash-set! seen key #t)
                 (set! acc (cons `(wire ,(list (car a) (cdr a))
                                        ,(list (car b) (cdr b))) acc))))))
	 links)
        (reverse acc)))
    (let ((body `(par ,@nodes-out ,@links-out)))
      (if showNu
          (let ((nu-names '()))
            (hash-for-each (lambda (nm _v) (set! nu-names (cons nm nu-names)))
			   (net-nu net))
            `(nu ,(reverse nu-names) ,body))
          body))))


