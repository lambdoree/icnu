(define-module (icnu tools icnu-inject)
  #:use-module (srfi srfi-1)
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:export (generate-injection-form))

(define (generate-injection-form initial-values)
  ;; Robust injection generator (no named-let; uses letrec for recursion)
  (let ((acc '()))
    (letrec* ((emit-list
               (lambda (lst)
                 (if (null? lst)
                     ;; empty list => emit NIL node
                     (let ((n (gensym "inj-nil-")))
                       (set! acc (cons (IC_NIL n) acc))
                       n)
                     ;; non-empty: reverse once, build cons-chain from tail forward
                     (let* ((rev  (reverse lst))
                            (tail (let ((n (gensym "inj-nil-")))
                                    (set! acc (cons (IC_NIL n) acc))
                                    n)))
                       (letrec ((cons-chain
                                 (lambda (elts tail-name)
                                   (if (null? elts)
                                       tail-name
                                       (let* ((head-name (make-val (car elts)))
                                              (cons-n   (gensym "inj-cons-")))
                                         (set! acc (cons (IC_CONS (list head-name 'p) tail-name cons-n) acc))
                                         (cons-chain (cdr elts) cons-n))))))
                         (cons-chain rev tail))))))
              (make-val
               (lambda (v)
                 (cond
                  ;; primitive literal -> IC_LITERAL
                  ((or (boolean? v) (number? v) (symbol? v) (string? v))
                   (let ((tmp (gensym "inj-lit-")))
                     (set! acc (cons (IC_LITERAL v tmp) acc))
                     tmp))
                  ;; list -> build cons chain
                  ((list? v)
                   (emit-list v))
                  ;; fallback: fresh node 'A'
                  (else
                   (let ((tm (gensym "inj-node-")))
                     (set! acc (cons (mk-node tm 'A) acc))
                     tm))))))
      ;; iterate requested injections and emit forms
      (for-each
       (lambda (pair)
         (when (pair? pair)
           (let ((id  (car pair))
                 (val (cdr pair)))
             (cond
              ((or (boolean? val) (number? val) (symbol? val) (string? val))
               (let ((tmp (gensym (string-append "inj-lit-" (symbol->string id) "-"))))
                 (set! acc (cons (IC_LITERAL val tmp) acc))
                 (set! acc (cons (mk-wire tmp 'p id 'r) acc))))
              ((list? val)
               (let ((vname (make-val val)))
                 (set! acc (cons (mk-wire vname 'p id 'r) acc))))
              (else
               (let ((tm (gensym "inj-node-")))
                 (set! acc (cons (mk-node tm 'A) acc))
                 (set! acc (cons (mk-wire tm 'p id 'r) acc))))))))
       initial-values)
      ;; Return combined par form (preserve original order)
      `(par ,@(reverse acc)))))
