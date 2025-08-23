(define-module (icnu utils format)
  #:export (format-string))

(define (format-string destination fmt . args)
  (let ((str (apply simple-format fmt args)))
    (if destination
        (begin (display str) (force-output))
        str)))

(define (simple-format fmt . args)
  (call-with-output-string
   (lambda (port)
     (let loop ((chars (string->list fmt)) (current-args args))
       (if (null? chars)
           'done
           (let ((c (car chars)))
             (if (and (char=? c #\~) (not (null? (cdr chars))))
                 (let ((spec (cadr chars)))
                   (case spec
                     ((#\a #\s)
                      (if (null? current-args)
                          (begin (display #\~ port) (display spec port) (loop (cddr chars) current-args))
                          (begin (display (car current-args) port) (loop (cddr chars) (cdr current-args)))))
                     ((#\~)
                      (begin (display #\~ port) (loop (cddr chars) current-args)))
                     (else
                      (begin (display c port) (loop (cdr chars) current-args)))))
                 (begin (display c port) (loop (cdr chars) current-args)))))))))
