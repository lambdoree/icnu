(define-module (icnu ic)
  #:use-module (icnu utils internal)
  #:use-module (srfi srfi-9)
  #:use-module (icnu utils format)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils log)
  #:use-module (icnu utils compat)
  #:use-module (ice-9 match)
  #:export (ic-parse-net ic-pretty-print
					          empty-net copy-net all-names node-agent endpoint valid-port?
					          peer net-nodes net-links net-tags net-meta get-ports unlink-port!
					          rewire! ic-delete-node! all-nodes-with-agent find-active-pairs
					          set-link-conflict-mode!
					          *link-conflict-mode*
					          mark-temporary! unmark-temporary! temporary-endpoint?
					          link-peers!
					          add-node! set-node-tag! node-tag set-node-meta! node-meta
					          ic-meta-get ic-meta-set!
					          <net>
					          net?
                    net-counter set-net-counter!
                    plist-put plist-remove
					          pp-bool
					          ))

(define-record-type <net>
  (make-net nodes links counter tags tmp-endpoints meta)
  net?
  (nodes net-nodes)
  (links net-links)
  (counter net-counter set-net-counter!)
  (tags net-tags)
  (tmp-endpoints net-tmp-endpoints)
  (meta net-meta))

(define (empty-net)
  (make-net
   (make-hash-table)          ; nodes
   (make-hash-table)          ; links
   0                          ; counter
   (make-hash-table)          ; tags
   (make-hash-table)          ; tmp-endpoints
   (make-hash-table)))        ; meta

(define (copy-net n)
  (let ((nn (make-hash-table))
        (ll (make-hash-table))
        (tags_copy (make-hash-table))
        (tmp_copy (make-hash-table))
        (meta_copy (make-hash-table)))
    (hash-for-each (lambda (k v) (hash-set! nn k v)) (net-nodes n))
    (hash-for-each
     (lambda (k v)
       (let ((k2 (if (pair? k) (cons (car k) (cdr k)) k))
             (v2 (if (pair? v) (cons (car v) (cdr v)) v)))
         (hash-set! ll k2 v2)))
     (net-links n))
    (hash-for-each (lambda (k v) (hash-set! tags_copy k v)) (net-tags n))
    (hash-for-each (lambda (k v) (hash-set! tmp_copy k v)) (net-tmp-endpoints n))
    (hash-for-each (lambda (k v) (hash-set! meta_copy k v)) (net-meta n))
    (make-net nn ll (net-counter n) tags_copy tmp_copy meta_copy)))

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

(define (set-node-tag! n name tag)
  (hash-set! (net-tags n) name tag))

(define (set-node-meta! n name val)
  (let ((store (if (eq? val #f) (cons 'value val) val)))
    (hash-set! (net-meta n) name store)))

(define (node-tag n name)
  (hash-ref (net-tags n) name 'user/opaque))

(define (node-meta n name)
  (hash-ref (net-meta n) name #f))

(define (node-agent n name)
  (hash-ref (net-nodes n) name #f))

(define (mark-temporary! net node port)
  (hash-set! (net-tmp-endpoints net) (endpoint node port) #t))

(define (unmark-temporary! net node port)
  (hash-remove! (net-tmp-endpoints net) (endpoint node port)))

(define (temporary-endpoint? net ep)
  (hash-ref (net-tmp-endpoints net) ep #f))

(define (endpoint name port)
  (when (not (valid-port? port)) (error "Invalid port" port))
  (cons name port))

(define *link-conflict-mode* (make-parameter 'error))

(define (set-link-conflict-mode! v)
  (if (memq v ' (error inject-temporary replace-exact))
      (*link-conflict-mode* v)
      (error "set-link-conflict-mode!: invalid mode" v)))

(define (link-peers! n a-port b-port)
  (let* ((L (net-links n))
         (pa (peer n a-port))
         (pb (peer n b-port))
         (mode (*link-conflict-mode*)))
    (case mode
      ((error)
       (if (and (or (not pa) (equal? pa b-port))
                (or (not pb) (equal? pb a-port)))
           (begin
             (hash-set! L a-port b-port)
             (hash-set! L b-port a-port))
           (error "link-peers! [error]: conflicting link"
                  (list 'a-port a-port 'peer pa)
                  (list 'b-port b-port 'peer pb))))
      ((replace-exact)
       (cond
        ((and (equal? pa b-port) (equal? pb a-port)) n) ; R1: idempotent
        ((and (equal? pa b-port) (not pb)) (hash-set! L b-port a-port)) ; R2: repair
        ((and (equal? pb a-port) (not pa)) (hash-set! L a-port b-port)) ; R2: repair
        ((and (not pa) (not pb))
         (error "replace-exact: cannot create new link" a-port b-port))
        (else
         (error "replace-exact: existing links differ from the exact pair"
                (list 'a a-port 'peer pa) (list 'b b-port 'peer pb)))))
      ((inject-temporary)
       (let ((ta (temporary-endpoint? n a-port))
             (tb (temporary-endpoint? n b-port)))
         (cond
          ((or ta tb)
           (when (and ta (not tb) pb (not (equal? pb a-port)))
             (error "inject-temporary: non-temp peer is busy" b-port pb))
           (when (and tb (not ta) pa (not (equal? pa b-port)))
             (error "inject-temporary: non-temp peer is busy" a-port pa))
           (when pa (unlink-port! n a-port))
           (when pb (unlink-port! n b-port))
           (hash-set! L a-port b-port)
           (hash-set! L b-port a-port))
          (else (error "inject-temporary: neither endpoint is temporary" a-port b-port)))))
      (else (error "link-peers!: unknown conflict mode" mode)))))

(define (unlink-port! n a-port)
  (let* ((L (net-links n))
         (b-port (peer n a-port)))
    (when b-port
      (hash-remove! L b-port)
      (let ((key-to-remove #f))
        (hash-for-each (lambda (k v)
                         (when (equal? k a-port)
                           (set! key-to-remove k)))
					             L)
        (when key-to-remove
          (hash-remove! L key-to-remove)))))
  n)

(define (peer n a-port)
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

(define (ic-delete-node! n x)
  (let ((agent (node-agent n x)))
    (when agent
      (for-each (lambda (pt) (unlink-port! n (cons x pt))) (get-ports agent))
      (hash-remove! (net-nodes n) x)
      (hash-remove! (net-tags n) x)
      (hash-remove! (net-meta n) x)))
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

(define (all-names n)
  (let ((s '()))
    (hash-for-each (lambda (k v) (set! s (cons k s))) (net-nodes n))
    s))

(define (plist-put plist key val)
  (let* ((base-plist (if (list? plist)
                         plist
                         (if (not plist) '() (list (cons 'value plist)))))
         (filtered (icnu-filter (lambda (p) (not (eq? (car p) key))) base-plist)))
    (cons (cons key val) filtered)))

(define (plist-remove plist key)
  (if (list? plist)
      (icnu-filter (lambda (p) (not (eq? (car p) key))) plist)
      plist))

;; General meta access helpers (core-provided, safe abstraction over node-meta)
(define (ic-meta-get net name key)
  (let ((meta (node-meta net name)))
    (if (and (list? meta) (assq key meta))
        (cdr (assq key meta))
        #f)))

(define (ic-meta-set! net name key val)
  (let ((meta (node-meta net name)))
    (set-node-meta! net name (plist-put meta key val))
    net))

(define (unquote-if-needed x)
  (if (and (pair? x) (eq? (car x) 'quote) (not (null? (cdr x))))
      (cadr x)
      x))

(define (ensure-free-name-node! n name)
  (unless (node-agent n name)
    (add-node! n name 'V)))

(define (parse-endpoint n ep)
  (let* ((pair (match ep
                 (('quote p) p)
                 (('list . p) p)
                 (_ ep)))
         (a-form (and (pair? pair) (car pair)))
         (p-form (and (pair? pair) (pair? (cdr pair)) (cadr pair))))
    (if (and a-form p-form)
        (let ((a (unquote-if-needed a-form))
              (p (unquote-if-needed p-form)))
          (unless (and (symbol? a) (valid-port? p))
            (error "parse: bad endpoint form" ep))
          (ensure-free-name-node! n a)
          (endpoint a p))
        (error "parse-endpoint: Malformed endpoint. Expected a list like ('node-name 'port) or (list 'node-name 'port)." ep))))

(define (parse-wire-args n args)
  (let ((len (length args)))
    (cond
     ((= len 4)
      (let ((a (unquote-if-needed (list-ref args 0)))
            (p (unquote-if-needed (list-ref args 1)))
            (b (unquote-if-needed (list-ref args 2)))
            (q (unquote-if-needed (list-ref args 3))))
        (unless (and (symbol? a) (symbol? p) (symbol? b) (symbol? q))
          (error "parse-wire-args: Bad 4-argument wire form. Expected (wire 'a 'p 'b 'q)." args))
        (values (endpoint a p) (endpoint b q))))
     ((= len 3)
      (if (list? (list-ref args 2)) ; (a p ep2) form
          (let ((a (unquote-if-needed (list-ref args 0)))
                (p (unquote-if-needed (list-ref args 1)))
                (ep2 (list-ref args 2)))
            (unless (and (symbol? a) (symbol? p)) (error "parse-wire-args: Bad 3-argument wire form. Expected (wire 'a 'p ('b 'q))." args))
            (values (endpoint a p) (parse-endpoint n ep2)))
                                        ; (ep1 b q) form
          (let ((ep1 (list-ref args 0))
                (b (unquote-if-needed (list-ref args 1)))
                (q (unquote-if-needed (list-ref args 2))))
            (unless (and (list? ep1) (symbol? b) (symbol? q))
              (error "parse-wire-args: Bad 3-argument wire form. Expected (wire ('a 'p) 'b 'q)." args))
            (values (parse-endpoint n ep1) (endpoint b q)))))
     ((= len 2)
      (let ((ep1 (list-ref args 0))
            (ep2 (list-ref args 1)))
        (values (parse-endpoint n ep1) (parse-endpoint n ep2))))
     (else
      (error "parse-wire-args: Invalid wire form. Must have 2, 3, or 4 arguments." args)))))

(define (parse-1 n form)
  (match form
    (('node name-form agent-form . rest)
     (let ((name (unquote-if-needed name-form))
           (agent (unquote-if-needed agent-form)))
       (when (and (symbol? name) (symbol? agent))
         (add-node! n name agent)
         (when (pair? rest)
           (let ((tag (unquote-if-needed (car rest))))
             (set-node-tag! n name tag)
             (when (pair? (cdr rest))
               (let ((meta (unquote-if-needed (cadr rest))))
                 (set-node-meta! n name meta)))))))
     n)
    (('wire . args)
     (call-with-values (lambda () (parse-wire-args n args))
       (lambda (e1 e2)
         (link-peers! n e1 e2)
         n)))
    (('par . es)
     (icnu-fold (lambda (form acc) (parse-1 acc form)) n es))
    (else (error "parse-1: Unrecognized form. Expected (node ...), (wire ...), or (par ...)." form))))

(define (pp-bool opt k dflt)
	(let ((v (assq-ref opt k)))
    (if (boolean? v) v dflt)))

(define (ic-pretty-print net . maybe-opts)
	(let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (showV (pp-bool opts 'show-V? #f))
         (showNu (pp-bool opts 'show-nu? #f))
         (showTags (pp-bool opts 'show-tags? #f))
         (showMeta (pp-bool opts 'show-meta? #t))
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
             (let* ((tag (node-tag net nm))
                    (has-tag (or showTags (not (eq? tag 'user/opaque))))
                    (sentinel (list 'sentinel))
                    (meta (hash-ref (net-meta net) nm sentinel))
                    (has-meta (and showMeta (not (eq? meta sentinel)))))
               (set! acc (cons (cond
                                (has-meta `(node ,nm ,ag ,tag ,meta))
                                (has-tag `(node ,nm ,ag ,tag))
                                (else `(node ,nm ,ag)))
                               acc)))))
         nodes)
        (reverse acc)))
    (define links-out
		  (let ((seen (make-hash-table))
            (acc '()))
        (hash-for-each
         (lambda (a b)
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
      ;; Nu printing logic will be in nu.scm or the bundle module
      body)))

(define (ic-parse-net sexpr)
  (parse-1 (empty-net) sexpr))

