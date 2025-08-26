(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu eval))

(define *long-tests-enabled* (make-parameter #f))

(define (test-long-church-apply_heavy)
  "Heavy reduction using IC_CHURCH-APPLY with a larger n.
   This test is gated behind *long-tests-enabled* and is skipped by default."
  (if (not (*long-tests-enabled*))
      (begin (format-string #t "skipping long test: heavy church-apply~%") #t)
      (let* ((n 30)
             (sexpr (read (open-input-string (format-string #f "~a" (IC_CHURCH-APPLY n 'f 'x 'outc))))))
        (let ((net (parse-net sexpr)))
          ;; run with an upper cap to be defensive in CI; when enabled, this will exercise longer reductions.
          (let ((reduced (reduce-net-to-normal-form net '((max-iter . 200)))))
            (assert-true (node-agent reduced 'outc) "outc node exists after heavy reduction")
            #t)))))

(define (run-all-long-tests)
  (let ((tests (list
                test-long-church-apply_heavy)))
    (display "Running long tests (skipped by default)...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (catch 'long-tests (lambda () (t) #t) (lambda args (display "EXN\n") #f))))
                  (if res (display "ok\n") (display "FAIL\n"))))
              tests)
    (display "Long tests completed (some may be skipped).\n")
    #t))

(run-all-long-tests)
