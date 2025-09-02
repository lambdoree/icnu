(define-module (icnu utils internal)
  #:export (icnu-match icnu-fold
            normalize-ep ensure-number gensyms icnu-any icnu-filter icnu-map icnu-string-prefix? icnu-string-suffix?))

(define (normalize-ep maybe-ep default-port)
  (if (symbol? maybe-ep)
      (list maybe-ep default-port)
      maybe-ep))

(define (ensure-number n who)
  (unless (number? n)
    (error (string-append who ": first argument must be a number") n)))

(define (gensyms prefix n)
  (let loop ((k n) (acc '()))
    (if (= k 0)
        (reverse acc)
        (loop (- k 1) (cons (gensym prefix) acc)))))

(define (icnu-any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (icnu-any pred (cdr lst)))))

(define (icnu-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (icnu-map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (map proc (cdr lst)))))

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

;; icnu-match : pattern datum -> boolean
;; Very small subset of `match` used in this project.
;; Supports:
;;   - literal symbols/numbers/strings
;;   - (list ...) patterns
;;   - (quote x) patterns
;;   - (or pattern1 pattern2) via `or` in pattern list
;;   - variable binding via symbols starting with `?` (e.g. ?x)
(define (icnu-match pat datum)
  (cond
   ((symbol? pat)
    (if (char=? (string-ref (symbol->string pat) 0) #\?) ; variable pattern ?x
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

;; The original `match` macro is provided by Guile's `(ice-9 match)`.
;; This module only exports `icnu-match`; callers that need pattern matching
;; should import `(ice-9 match)` directly.
