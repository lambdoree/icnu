(define-module (icnu tools icnu-big-step)
  #:use-module (icnu tools icnu-proof)
  #:export (big-step-output))

(define (big-step-output src)
  (big-step-string src))
