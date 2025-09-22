(define-module (icnu utils log)
  #:use-module (icnu utils format)
  #:use-module (icnu utils compat)
  #:export (debug-level? set-debug-level! set-debug-log!
           debugf warnf))

(define *debug-level* (icnu-make-parameter 0))

(define (debug-level?) (*debug-level*))

(define (set-debug-level! n) (*debug-level* n))

(define (set-debug-log! v)
  (if (boolean? v)
      (set-debug-level! (if v 1 0))
      (set-debug-level! v)))

(define (debugf level fmt . args)
  (if (>= (debug-level?) level)
      (apply format-string (cons (current-output-port) (cons fmt args)))
      #t))

(define (warnf fmt . args)
  (apply debugf (cons 1 (cons fmt args))))




