(define-module (icnu icnu-rewrite)
  #:use-module (icnu utils internal)
  #:use-module (icnu utils compat)
  #:use-module (icnu utils strings)
  #:use-module (ice-9 match)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu icnu)
  #:use-module ((icnu literals) #:prefix lit:)
  #:export (resolve-literal-ep
            is-literal-node?
            get-literal-value
            *unresolved*
            *resolve-literal-limit*
            rewrite-pass-if-fold!
            rewrite-pass-const-fold!))

(define (is-literal-node? net node-name)
  (lit:ic-literal? net node-name))

(define (get-literal-value net node-name)
  (lit:ic-literal-value net node-name))

(define *unresolved* (string->symbol "icnu-unresolved"))

(define *resolve-literal-limit* 4096)

(define (ep-key ep)
  (if (and (pair? ep) (symbol? (car ep)) (symbol? (cdr ep)))
      (string-append (symbol->string (car ep)) "|" (symbol->string (cdr ep)))
      (format-string #f "~a" ep)))

(define (peers-of net n)
  (values (peer net (cons n 'p))
          (peer net (cons n 'l))
          (peer net (cons n 'r))))

(define (follow-port-peer net n port)
  (case port
    ((p) (peer net (cons n 'p)))
    ((l) (peer net (cons n 'l)))
    ((r) (peer net (cons n 'r)))
    (else #f)))


(define (resolve-from-A-node net n current-port k seen recur)
  (let* ((tag (node-tag net n)))
    (if (memq tag '(prim/eq prim/lt prim/gt prim/add prim/sum1 prim/if))
        *unresolved*
        (call-with-values
            (lambda () (peers-of net n))
          (lambda (p-peer l-peer r-peer)
            (let ((next (follow-port-peer net n current-port)))
              (cond
               (next
                (let ((res (recur next (- k 1) seen)))
                  (if (not (eq? res *unresolved*)) res *unresolved*)))
               ((and (not l-peer) (not r-peer) p-peer)
                (let ((res (recur p-peer (- k 1) seen)))
                  (if (not (eq? res *unresolved*)) res *unresolved*)))
               ((and (eq? current-port 'p) l-peer r-peer)
                (let* ((l-val
                        (let ((res (recur l-peer (- k 1) seen)))
                          (if (not (eq? res *unresolved*)) res
                              (if (and (pair? l-peer) (symbol? (car l-peer))) l-peer *unresolved*))))
                       (r-val
                        (let ((res (recur r-peer (- k 1) seen)))
                          (if (not (eq? res *unresolved*)) res
                              (if (and (pair? r-peer) (symbol? (car r-peer))) r-peer *unresolved*)))))
                  (cond
                   ((and (pair? l-val) (symbol? (car l-val)) (valid-port? (cdr l-val)) (not (eq? r-val *unresolved*)))
                    r-val)
                   ((and (pair? r-val) (symbol? (car r-val)) (valid-port? (cdr r-val)) (not (eq? l-val *unresolved*)))
                    l-val)
                   (else *unresolved*))))
               (else
                (let ((p-res (if p-peer (recur p-peer (- k 1) seen) *unresolved*)))
                  (if (not (eq? p-res *unresolved*)) p-res *unresolved*))))))))))



(define (resolve-literal-ep* net ep k seen)
  (letrec ((recur
            (lambda (current-ep kk s)
              (if (or (not current-ep) (<= kk 0))
                  *unresolved*
                  (let ((key (ep-key current-ep)))
                    (if (hash-ref s key #f)
                        *unresolved*
                        (begin
                          (hash-set! s key #t)
                          (if (and (pair? current-ep) (symbol? (car current-ep)))
                              (let* ((n (car current-ep))
                                     (p-rest (cdr current-ep))
                                     (port (if (symbol? p-rest) p-rest
                                               (and (pair? p-rest) (null? (cdr p-rest)) (car p-rest))))
                                     (agent (node-agent net n)))
                                (cond
                                 ((is-literal-node? net n)
                                  (get-literal-value net n))

                                 ((eq? agent 'A)
                                  (resolve-from-A-node net n port kk s recur))

                                 ((eq? agent 'C)
                                  (let ((p-peer (peer net (cons n 'p))))
                                    (if p-peer (recur p-peer (- kk 1) s) *unresolved*)))

                                 (else
                                  (let ((p (peer net current-ep)))
                                    (if p (recur p (- kk 1) s) *unresolved*)))))
                              *unresolved*))))))))
    (recur ep k seen)))

(define (resolve-literal-ep net ep . maybe-limit)
  (let ((limit (if (null? maybe-limit) *resolve-literal-limit* (car maybe-limit)))
        (seen (make-hash-table)))
    (resolve-literal-ep* net ep limit seen)))

;; Non-pure constant folding and if-folding use Scheme semantics

(define (ensure-global-lit-num-node net val)
  (let ((name (icnu-gensym "lit-num-")))
    (lit:ic-make-literal-node! net name 'lit/num val)
    (mark-nu! net name)
    name))

(define (ensure-global-bool-node net val)
  (ensure-global-lit-num-node net (if val 1 0)))

(define (ensure-global-num-node net val)
  (ensure-global-lit-num-node net val))

(define (rewrite-pass-if-fold! net)
  (let ((changed? #f))
    (for-each
     (lambda (if-name)
       (when (eq? (node-tag net if-name) 'prim/if)
         (let* ((p-peer (peer net (cons if-name 'p)))
                (cond-copy (and p-peer (car p-peer)))
                (cond-ep (and cond-copy (peer net (cons cond-copy 'p))))
                (cond-val (and cond-ep (resolve-literal-ep net cond-ep *resolve-literal-limit*))))
           (let* ((cond-bool
                   (cond
                    ((boolean? cond-val) cond-val)
                    ((number? cond-val) (not (= cond-val 0)))
                    ((string? cond-val) (> (string-length cond-val) 0))
                    (else *unresolved*))))
             (when (not (eq? cond-bool *unresolved*))
               (let* ((kept-port (if cond-bool 'l 'r))
                      (pruned-port (if cond-bool 'r 'l))
                      (kept-branch-ep (peer net (cons if-name kept-port)))
                      (pruned-branch-ep (peer net (cons if-name pruned-port)))
                      (output-dest (and cond-copy (peer net (cons cond-copy 'r)))))
                 (let ((kept-copier (and kept-branch-ep (car kept-branch-ep))))
                   (when (and kept-copier output-dest)
                     (let ((value-source (peer net (cons kept-copier 'p))))
                       (when value-source
                         (let ((source-node-name (car value-source)))
                           (if (is-literal-node? net source-node-name)
                               (rewire! net output-dest value-source)
                               (let ((real-source (peer net value-source)))
                                 (when real-source
                                   (rewire! net output-dest real-source)))))))))
                 (when pruned-branch-ep (delete-node! net (car pruned-branch-ep)))
                 (delete-node! net if-name)
                 (when cond-copy (delete-node! net cond-copy))
                 (set! changed? #t)))))))
     (all-nodes-with-agent net 'A))
    changed?))

(define (rewrite-pass-const-fold! net)
  (let ((changed? #f))
    (for-each
     (lambda (n)
       (let ((tag (node-tag net n)))
         (when (memq tag '(prim/eq prim/lt prim/gt prim/add prim/sum1))
           (let* ((l-ep (peer net (cons n 'l)))
                  (r-ep (peer net (cons n 'r)))
                  (l-val (if l-ep (resolve-literal-ep net l-ep *resolve-literal-limit*) *unresolved*))
                  (r-val (if r-ep (resolve-literal-ep net r-ep *resolve-literal-limit*) *unresolved*)))
             (let ((res
                    (cond
                     ((eq? tag 'prim/lt)
                      (if (and (number? l-val) (number? r-val)) (< l-val r-val) *unresolved*))
                     ((eq? tag 'prim/gt)
                      (if (and (number? l-val) (number? r-val)) (> l-val r-val) *unresolved*))
                     ((eq? tag 'prim/eq)
                      (if (and (not (eq? l-val *unresolved*)) (not (eq? r-val *unresolved*)))
                          (equal? l-val r-val)
                          *unresolved*))
                     ((eq? tag 'prim/add)
                      (if (and (number? l-val) (number? r-val)) (+ l-val r-val) *unresolved*))
                     ((eq? tag 'prim/sum1)
                      (if (number? l-val)
                          (let* ((n l-val))
                            (if (<= n 0) 0 (/ (* n (+ n 1)) 2)))
                          *unresolved*))
                     (else *unresolved*))))
               (when (or (boolean? res) (number? res))
                 (let ((lit (if (boolean? res)
                                (ensure-global-bool-node net res)
                                (ensure-global-num-node net res)))
                       (out-ep (peer net (cons n 'p))))
                   (when out-ep (rewire! net out-ep (cons lit 'p)))
                   (delete-node! net n)
                   (set! changed? #t)
                   (debugf 1 "rewrite-pass-const-fold!: folded ~a -> ~a\n" n res))))))))
     (all-nodes-with-agent net 'A))
    changed?))
