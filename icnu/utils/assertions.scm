(define-module (icnu utils assertions)
  #:export (assert-eq assert-true assert-false))

(define (assert-eq a b msg)
  (unless (equal? a b)
    (error "assert-eq failed:" msg a b))
  #t)

(define (assert-true v msg)
  (unless v (error "assert-true failed:" msg))
  #t)

(define (assert-false v msg)
  (when v (error "assert-false failed:" msg))
  #t)

