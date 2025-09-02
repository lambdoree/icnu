(define-module (icnu utils helpers)
  #:use-module (icnu icnu)
  #:export (wire-or-list icnu-wire-or-list))

(define (wire-or-list src dst . maybe-port)
  (let ((port (if (null? maybe-port) 'p (car maybe-port))))
    (if (symbol? src)
        (mk-wire src 'p dst port)
        (list 'wire src (list dst port)))))

;; New wrapper with icnu- prefix
(define (icnu-wire-or-list src dst . maybe-port)
  (apply wire-or-list src dst maybe-port))
