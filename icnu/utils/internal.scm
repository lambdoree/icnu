(define-module (icnu utils internal)
  #:use-module (icnu utils compat)
  #:export (icnu-match icnu-fold
                       icnu-normalize-ep icnu-ensure-number icnu-any icnu-filter icnu-map icnu-string-prefix? icnu-string-suffix?
                       icnu-andmap))

(define (icnu-normalize-ep maybe-ep default-port)
  (if (symbol? maybe-ep)
      (list maybe-ep default-port)
      maybe-ep))

(define (icnu-ensure-number n who)
  (if (not (number? n))
      (error (string-append who ": first argument must be a number") n)))

(define (icnu-any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (icnu-any pred (cdr lst)))))

(define (icnu-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (icnu-filter pred (cdr lst))))
        (else (icnu-filter pred (cdr lst)))))

(define (icnu-map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (icnu-map proc (cdr lst)))))

(define (icnu-fold proc init lst)
  (let loop ((l lst) (acc init))
    (if (null? l)
        acc
        (loop (cdr l) (proc (car l) acc)))))

(define (icnu-string-prefix? prefix str)
  (let ((plen (string-length prefix))
        (slen (string-length str)))
    (and (<= plen slen)
         (string=? prefix (substring str 0 plen)))))

(define (icnu-string-suffix? suffix str)
  (let ((slen (string-length str))
        (suflen (string-length suffix)))
    (and (<= suflen slen)
         (string=? suffix (substring str (- slen suflen) slen)))))

(define (icnu-match pat datum)
  (cond
   ((symbol? pat)
    (if (char=? (string-ref (symbol->string pat) 0) #\?)
        #t
        (eq? pat datum)))
   ((pair? pat)
    (cond
     ((eq? (car pat) 'quote)
      (eq? (cadr pat) datum))
     ((eq? (car pat) 'list)
      (and (pair? datum)
           (icnu-match (cdr pat) datum)))
     (else
      (and (pair? datum)
           (icnu-match (car pat) (car datum))
           (icnu-match (cdr pat) (cdr datum))))))
   (else (equal? pat datum))))

(define (icnu-andmap p . lists)
  (define (any-null? ls)
    (cond ((null? ls) #f)
          ((null? (car ls)) #t)
          (else (any-null? (cdr ls)))))

  (cond
    ((null? lists) #t)
    (else
     (let loop ((ls lists))
       (if (any-null? ls)
           #t
           (let ((ok (apply p (map car ls))))
             (if ok
                 (loop (map cdr ls))
                 #f)))))))

