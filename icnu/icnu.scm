(define-module (icnu icnu)
  #:use-module ((icnu ic) #:prefix ic:)
  #:use-module (icnu utils log)
  #:use-module (ice-9 match)
  #:use-module (icnu utils internal)
  #:use-module ((icnu literals) #:prefix lit:)
  #:export (
            ;; S-expression constructors
            mk-node-labeled
            
            ;; Net lifecycle and inspection
            parse-net pretty-print empty-net copy-net
            all-names all-nodes-with-agent find-active-pairs
            
            ;; Node and link properties and manipulation
            add-node! delete-node! rewire! link-peers! unlink-port!
            node-agent get-ports valid-port? endpoint peer
            net-nodes net-links net-tags net-meta

            ;; Node metadata (tags, etc.)
            set-node-tag! node-tag set-node-meta! node-meta
            ic-meta-get ic-meta-set! ic-literal? ic-literal-value ic-make-literal-node!
            
            ;; nu-specific operations
            node-nu? mark-nu! unmark-nu! inherit-nu!

            ;; Fresh name generation
            make-fresh-name
            
            ;; Link conflict handling
            set-link-conflict-mode! *link-conflict-mode*
            mark-temporary! unmark-temporary! temporary-endpoint?
            
            ;; Record type and predicates
            <net> net?

            ;; Utility
            plist-put plist-remove pp-bool
            ))

;;; --------------------------------------------------------------------
;;; Nu-specific helpers
;;; --------------------------------------------------------------------

(define (node-nu? net name)
  (let ((v (ic:ic-meta-get net name 'nu?)))
    (and v (not (eq? v #f)))))

(define (unmark-nu! net name)
  (ic:ic-meta-set! net name 'nu? #f)
  net)

(define (mark-nu! net name)
  (debugf 2 "mark-nu!: marking ~a as nu-bound\n" name)
  (ic:ic-meta-set! net name 'nu? #t)
  net)

(define (inherit-nu! net new-node . parent-nodes)
  (when (icnu-any (lambda (p) (node-nu? net p)) parent-nodes)
    (debugf 2 "inherit-nu!: ~a inherits nu from ~a\n" new-node parent-nodes)
    (mark-nu! net new-node)))

;;; --------------------------------------------------------------------
;;; Core functions extended with nu-awareness
;;; --------------------------------------------------------------------
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
        (error "parse-endpoint: Malformed endpoint." ep))))

(define (parse-wire-args n args)
  (let ((len (length args)))
    (cond
     ((= len 4)
      (let ((a (unquote-if-needed (list-ref args 0)))
            (p (unquote-if-needed (list-ref args 1)))
            (b (unquote-if-needed (list-ref args 2)))
            (q (unquote-if-needed (list-ref args 3))))
        (unless (and (symbol? a) (symbol? p) (symbol? b) (symbol? q))
          (error "parse-wire-args: Bad 4-argument wire form." args))
        (values (endpoint a p) (endpoint b q))))
     ((= len 3)
      (if (list? (list-ref args 2)) ; (a p ep2) form
          (let ((a (unquote-if-needed (list-ref args 0)))
                (p (unquote-if-needed (list-ref args 1)))
                (ep2 (list-ref args 2)))
            (unless (and (symbol? a) (symbol? p)) (error "parse-wire-args: Bad 3-argument wire form." args))
            (values (endpoint a p) (parse-endpoint n ep2)))
          (let ((ep1 (list-ref args 0))
                (b (unquote-if-needed (list-ref args 1)))
                (q (unquote-if-needed (list-ref args 2))))
            (unless (and (list? ep1) (symbol? b) (symbol? q))
              (error "parse-wire-args: Bad 3-argument wire form." args))
            (values (parse-endpoint n ep1) (endpoint b q)))))
     ((= len 2)
      (let ((ep1 (list-ref args 0))
            (ep2 (list-ref args 1)))
        (values (parse-endpoint n ep1) (parse-endpoint n ep2))))
     (else
      (error "parse-wire-args: Invalid wire form." args)))))

(define (parse-1/nu n form use-nu?)
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
     (icnu-fold (lambda (form acc) (parse-1/nu acc form use-nu?)) n es))
    (('nu names-form body)
     (if use-nu?
         (let ((names (unquote-if-needed names-form)))
           (unless (list? names) (error "parse: nu names must be a list" names-form))
           (for-each (lambda (nm) (mark-nu! n nm)) names)
           (parse-1/nu n body use-nu?))
         (error "nu binder encountered, but nu-support is disabled" form)))
    (else (error "parse-1/nu: Unrecognized form." form))))

(define (parse-net/nu sexpr use-nu?)
  (parse-1/nu (empty-net) sexpr use-nu?))

(define (parse-net sexpr . maybe-use-nu)
  (let ((use-nu? (if (null? maybe-use-nu) #t (car maybe-use-nu))))
    (if use-nu?
        (parse-net/nu sexpr #t)
        (ic:ic-parse-net sexpr))))

(define (pretty-print net . maybe-opts)
  (let* ((body (apply ic:ic-pretty-print net maybe-opts))
         (opts (if (null? maybe-opts) '() (car maybe-opts)))
         (showNu (ic:pp-bool opts 'show-nu? #f)))
    (if showNu
        (let ((nu-names '()))
          (hash-for-each
           (lambda (nm ag)
             (when (node-nu? net nm)
               (set! nu-names (cons nm nu-names))))
           (ic:net-nodes net))
          (if (null? nu-names)
              body
              `(nu ,(reverse nu-names) ,body)))
        body)))

(define (delete-node! n x)
  (if (node-nu? n x)
      (let ((agent (node-agent n x)))
        (when agent
          (for-each (lambda (pt) (unlink-port! n (cons x pt))) (get-ports agent))
          (hash-set! (ic:net-nodes n) x 'V)
          (hash-set! (ic:net-tags n) x 'user/opaque))
        n)
      (ic:ic-delete-node! n x)))

;;; --------------------------------------------------------------------
;;; Wrappers for pure IC functions from (icnu ic)
;;; --------------------------------------------------------------------
(define (mk-node-labeled name agent . rest)
  (let ((tag #f)
        (desc #f))
    (when (and (pair? rest) (not (null? rest)))
      (set! tag (car rest))
      (when (and (pair? (cdr rest)) (not (null? (cdr rest))))
        (set! desc (cadr rest))))
    `(node ,name ,agent ,@(cond ((and tag desc) (list tag desc))
                                ((and tag (not (eq? tag #f))) (list tag))
                                (desc (list 'user/opaque desc))
                                (else '())))))
(define empty-net ic:empty-net)
(define copy-net ic:copy-net)
(define all-names ic:all-names)
(define node-agent ic:node-agent)
(define endpoint ic:endpoint)
(define valid-port? ic:valid-port?)
(define peer ic:peer)
(define net-nodes ic:net-nodes)
(define net-links ic:net-links)
(define net-tags ic:net-tags)
(define net-meta ic:net-meta)
(define get-ports ic:get-ports)
(define unlink-port! ic:unlink-port!)
(define rewire! ic:rewire!)
(define all-nodes-with-agent ic:all-nodes-with-agent)
(define find-active-pairs ic:find-active-pairs)
(define set-link-conflict-mode! ic:set-link-conflict-mode!)
(define *link-conflict-mode* ic:*link-conflict-mode*)
(define mark-temporary! ic:mark-temporary!)
(define unmark-temporary! ic:unmark-temporary!)
(define temporary-endpoint? ic:temporary-endpoint?)
(define link-peers! ic:link-peers!)
(define add-node! ic:add-node!)
(define set-node-tag! ic:set-node-tag!)
(define node-tag ic:node-tag)
(define set-node-meta! ic:set-node-meta!)
(define node-meta ic:node-meta)
(define ic-meta-get ic:ic-meta-get)
(define ic-meta-set! ic:ic-meta-set!)

;; Literal helpers are provided by (icnu literals) and are imported with prefix 'lit:'.
;; We expose canonical icnu-facing wrappers so callers use `ic-literal?` etc via (icnu icnu),
;; while the implementation lives in (icnu literals) under the 'lit:' prefix.
(define (ic-literal? net name)
  (lit:ic-literal? net name))

(define (ic-literal-value net name)
  (lit:ic-literal-value net name))

(define (ic-make-literal-node! net name lit-tag lit-val)
  (lit:ic-make-literal-node! net name lit-tag lit-val))

(define <net> ic:<net>)
(define net? ic:net?)
(define plist-put ic:plist-put)
(define plist-remove ic:plist-remove)
(define pp-bool ic:pp-bool)

(define (make-fresh-name n prefix)
  (let* ((c (ic:net-counter n))
         (name (string->symbol (string-append prefix "-" (number->string c)))))
    (ic:set-net-counter! n (+ c 1))
    name))

