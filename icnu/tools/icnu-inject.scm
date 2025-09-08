(define-module (icnu tools icnu-inject)
  #:use-module (icnu utils internal)
  #:use-module (icnu utils compat)
  ;; compat: using icnu-gensym from compat module
  #:use-module (icnu icnu)
  #:use-module (icnu stdlib icnu-lib)
  #:export (generate-injection-form))

(define (gensym . maybe-prefix)
  (apply icnu-gensym maybe-prefix))

(define (generate-injection-form initial-values)
  (let ((acc '()))
    (letrec* ((emit-list
               (lambda (lst)
                 (if (null? lst)
                     (let ((n (gensym "inj-nil-")))
                       (set! acc (cons (IC_NIL n) acc))
                       n)
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
                  ((or (boolean? v) (number? v) (symbol? v) (string? v))
                   (let ((tmp (gensym "inj-lit-")))
                     (set! acc (cons (IC_LITERAL v tmp) acc))
                     tmp))
                  ((list? v)
                   (emit-list v))
                  (else
                   (let ((tm (gensym "inj-node-")))
                     (set! acc (cons (mk-node tm 'A) acc))
                     tm))))))
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
      `(par ,@(reverse acc)))))
