(define-module (tests test-runner)
  #:use-module (icnu utils format)
  #:export (run-tests))

(define (run-tests suite-name tests)
  (display (format-string #f "\nRunning ~a tests...\n" suite-name))
  (let ((failures '()))
    (for-each
     (lambda (t)
       (format-string #t " - ~a ... " (format-string #f "~a" t))
       (let ((res (catch #t
                         (lambda () (t) #t)
                         (lambda (key . args)
                           (format-string #t "EXCEPTION: ~a~%~a\n" key args)
                           #f))))
         (if res
             (display "ok\n")
             (begin
               (display "FAIL\n")
               (set! failures (cons (format-string #f "~a" t) failures))))))
     tests)
    (if (null? failures)
        (begin
          (display (format-string #f "All ~a tests completed successfully.\n" suite-name))
          #t)
        (begin
          (display (format-string #f "\n~a FAILED: ~a\n" suite-name (length failures)))
          (for-each (lambda (f) (format-string #t "  - ~a\n" f)) (reverse failures))
          #f))))
