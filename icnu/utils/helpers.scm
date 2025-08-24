(define-module (icnu utils helpers)
  #:use-module (icnu icnu)
  #:export (wire-or-list starts-with?))

(define (wire-or-list src dst)
  (if (symbol? src)
      (mk-wire src 'p dst 'p)
      (list 'wire src (list dst 'p))))

(define (starts-with? s prefix)
  (let ((ls (string-length s))
        (lp (string-length prefix)))
    (and (>= ls lp)
         (string=? (substring s 0 lp) prefix))))
