(define-module (icnu utils format)
  #:export (format-string))

(define (simple-format fmt . args)
  (let ((out (open-output-string)))
    (let loop ((chars (string->list fmt)) (current-args args))
      (if (null? chars)
          (get-output-string out)
          (let ((c (car chars)))
            (if (and (char=? c #\~) (not (null? (cdr chars))))
                (let ((spec (cadr chars)))
                  (case spec
                    ((#\a #\s)
                     (if (null? current-args)
                         (begin (write-char #\~ out) (write-char spec out) (loop (cddr chars) current-args))
                         (begin (display (car current-args) out) (loop (cddr chars) (cdr current-args)))))
                    ((#\~)
                     (begin (write-char #\~ out) (loop (cddr chars) current-args)))
                    ((#\%)
                     (begin (newline out) (loop (cddr chars) current-args)))
                    (else
                     (begin (display c out) (loop (cdr chars) current-args)))))
                (begin (display c out) (loop (cdr chars) current-args))))))))

(define (format-string destination fmt . args)
  (let ((str (apply simple-format fmt args)))
    (cond
     ((eq? destination #f) str)
     ((eq? destination #t) (begin (display str) str))
     ((not (boolean? destination)) (begin (display str destination) str))
     (else (begin (display str) str)))))

