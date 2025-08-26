(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu tools icnu-validate))

(define (test-validate-ir-valid)
  (let ((net (parse-net '(par (node a A) (node b A) (wire (a p) (b p))))))
    (assert-true (null? (validate-ir net)) "validate-ir on a valid net should return an empty list")))

(define (test-validate-ir-invalid-agent)
  (let ((net (empty-net)))
    (hash-set! (net-nodes net) 'x 'BADAGENT)
    (assert-false (null? (validate-ir net)) "validate-ir on a net with invalid agent should return a non-empty list")))

(define (test-validate-ir-non-reciprocal-link)
  (let ((net (empty-net)))
    (add-node! net 'a 'A)
    (add-node! net 'b 'A)
    ;; Manually create a non-reciprocal link
    (hash-set! (net-links net) (endpoint 'a 'p) (endpoint 'b 'p))
    (assert-false (null? (validate-ir net)) "validate-ir on a net with non-reciprocal link should return a non-empty list")))

(define (run-all-validation-tests)
  (let ((tests (list
                test-validate-ir-valid
                test-validate-ir-invalid-agent
                test-validate-ir-non-reciprocal-link)))
    (display "Running validation tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res
                      (display "ok\n")
                      (display "FAIL\n"))))
              tests)
    (display "All validation tests completed.\n")
    #t))

(run-all-validation-tests)
