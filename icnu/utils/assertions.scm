(define-module (icnu utils assertions)
  #:export (assert-eq assert-true assert-false
           icnu-assert-eq icnu-assert-true icnu-assert-false))

(define (assert-eq a b msg)
  (unless (equal? a b)
    (error "assert-eq failed:" msg a b))
  #t)

;; New wrapper with icnu- prefix
(define (icnu-assert-eq a b msg)
  (assert-eq a b msg))

(define (assert-true v msg)
  (unless v (error "assert-true failed:" msg))
  #t)

;; New wrapper with icnu- prefix
(define (icnu-assert-true v msg)
  (assert-true v msg))

(define (assert-false v msg)
  (when v (error "assert-false failed:" msg))
  #t)

;; New wrapper with icnu- prefix
(define (icnu-assert-false v msg)
  (assert-false v msg))
