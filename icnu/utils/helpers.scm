(define-module (icnu utils helpers)
  #:use-module (icnu icnu)
  #:export (wire-or-list))

(define (wire-or-list src dst . maybe-port)
  (let ((port (if (null? maybe-port) 'p (car maybe-port))))
    (if (symbol? src)
        `(wire (,src p) (,dst ,port))
        `(wire ,src (,dst ,port)))))

