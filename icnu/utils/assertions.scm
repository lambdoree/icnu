(define-module (icnu utils assertions)
  #:export (assert-eq assert-true assert-false))

(define (assert-eq a b msg)
  (if (equal? a b)
      #t
      (error "assert-eq failed:" msg a b)))

(define (assert-true v msg)
  (if v
      #t
      (error "assert-true failed:" msg)))

(define (assert-false v msg)
  (if v
      (error "assert-false failed:" msg)
      #t))

