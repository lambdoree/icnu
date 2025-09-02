(define-module (icnu utils format)
  #:export (format-string icnu-format-string))

(define (format-string destination fmt . args)
  (let ((str (apply simple-format fmt args)))
    (cond
     ((eq? destination #f)
      str)
     ((eq? destination #t)
      (begin (display str) (force-output) str))
     ((not (boolean? destination))
      (begin (display str destination) (force-output) str))
     (else
      (begin (display str) (force-output) str)))))

;; New wrapper with icnu- prefix
(define (icnu-format-string destination fmt . args)
  (apply format-string destination fmt args))

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
                     ((#\%)
                      (begin (newline port) (loop (cddr chars) current-args)))
                     (else
                      (begin (display c port) (loop (cdr chars) current-args)))))
                 (begin (display c port) (loop (cdr chars) current-args)))))))))
