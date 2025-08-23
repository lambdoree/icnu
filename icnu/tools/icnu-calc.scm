(define-module (icnu tools icnu-calc)
  #:use-module (srfi srfi-1)
  #:export (calc-eval calc-eval-str))

(define (calc-eval expr)
  (let ((res (eval expr (interaction-environment))))
    (if (number? res) res (error "calc-eval: non-numeric result" res))))

(define (calc-eval-str s)
  (call-with-input-string s
    (lambda (in)
      (let ((sexpr (read in)))
        (calc-eval sexpr)))))
