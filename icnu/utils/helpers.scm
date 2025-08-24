(define-module (icnu utils helpers)
  #:use-module (icnu icnu)
  #:export (wire-or-list))

(define (wire-or-list src dst)
  (if (symbol? src)
      (mk-wire src 'p dst 'p)
      (list 'wire src (list dst 'p))))
