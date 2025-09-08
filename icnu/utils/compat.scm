(define-module (icnu utils compat)
  #:export (icnu-make-hash-table icnu-hash-set! icnu-hash-ref icnu-hash-remove! icnu-hash-for-each
           icnu-gensym icnu-gensyms icnu-make-parameter))

(define (icnu-make-hash-table) (vector '()))

(define (alist-remove-key alist key)
  (let loop ((lst alist) (acc '()))
    (if (null? lst)
        (reverse acc)
        (let ((pair (car lst)))
          (if (equal? (car pair) key)
              (loop (cdr lst) acc)
              (loop (cdr lst) (cons pair acc)))))))

(define (icnu-hash-set! table key val)
  (let ((alist (vector-ref table 0)))
    (vector-set! table 0 (cons (cons key val) (alist-remove-key alist key)))
    table))

(define (icnu-hash-ref table key . maybe-default)
  (let ((alist (vector-ref table 0)))
    (let loop ((lst alist))
      (cond
        ((null? lst) (if (null? maybe-default) (error "icnu-hash-ref: key not found" key) (car maybe-default)))
        (else (let ((pair (car lst)))
                (if (equal? (car pair) key)
                    (cdr pair)
                    (loop (cdr lst)))))))))

(define (icnu-hash-remove! table key)
  (vector-set! table 0 (alist-remove-key (vector-ref table 0) key))
  table)

(define (icnu-hash-for-each proc table)
  (let loop ((lst (vector-ref table 0)))
    (if (null? lst)
        #t
        (begin
          (let ((p (car lst)))
            (proc (car p) (cdr p)))
          (loop (cdr lst)))))
  #t)

(define icnu-gensym-counter (vector 0))
(define (icnu-gensym . maybe-prefix)
  (let ((prefix (if (null? maybe-prefix) "g" (car maybe-prefix))))
    (vector-set! icnu-gensym-counter 0 (+ 1 (vector-ref icnu-gensym-counter 0)))
    (string->symbol (string-append prefix (number->string (vector-ref icnu-gensym-counter 0))))))

(define (icnu-gensyms prefix n)
  (let loop ((k n) (acc '()))
    (if (= k 0) (reverse acc)
        (loop (- k 1) (cons (icnu-gensym prefix) acc)))))

(define (icnu-make-parameter init)
  (let ((box (vector init)))
    (lambda args
      (cond
        ((null? args) (vector-ref box 0))
        ((= (length args) 1) (let ((v (car args))) (vector-set! box 0 v) v))
        (else (error "icnu-parameter: invalid arity" args))))))
