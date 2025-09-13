(use-modules (icnu icnu)
             (icnu utils debug)
             (icnu utils assertions)
             (icnu utils format)
             (icnu tools icnu-proof)
             (icnu utils log)
             (icnu utils strings)
             (tests test-runner))

(set-debug-level! 0)

(define (test-net-summary-string)
  (let ((net (parse-net '(par (node a A) (node c C) (node e E) (node v V)))))
    (assert-eq (net-summary-string net) "A:1 C:1 E:1 V:1" "net-summary-string with mixed agents"))
  (let ((net (empty-net)))
    (assert-eq (net-summary-string net) "A:0 C:0 E:0 V:0" "net-summary-string on empty net"))
  #t)

(define (test-dump-links)
  (let ((net (parse-net '(par (node a A) (node b A) (wire (a p) (b l))))))
    (assert-true (string-contains? (dump-links net) "a/p -> b/l") "dump-links shows simple link"))
  (let ((net (empty-net)))
    (assert-eq (dump-links net) "no links" "dump-links on net with no links"))
  #t)

(define (test-print-step-sequence)
  (let* ((net (parse-net '(par (node a A) (node b A) (wire (a p) (b p)))))
         (seq (list net (small-step-net net))))
    (let ((output (call-with-output-string
                   (lambda (p)
                     (parameterize ((current-output-port p))
                       (print-step-sequence seq))))))
      (assert-true (string-contains? output "---- Step 0 ----") "print-step-sequence shows step 0")
      (assert-true (string-contains? output "---- Step 1 ----") "print-step-sequence shows step 1")
      (assert-true (string-contains? output "Summary: A:2 C:0 E:0 V:0") "print-step-sequence shows initial summary")
      (assert-true (string-contains? output "Active pairs (count: 1)") "print-step-sequence shows active pairs for step 0")))
  #t)

(define (test-time-thunk)
  (let* ((result #f)
         (output (call-with-output-string
                  (lambda (p)
                    (parameterize ((current-output-port p))
                      (set! result (time-thunk (lambda () 42))))))))
    (assert-eq result 42 "time-thunk should return the thunk's result")
    (assert-true (string-contains? output "elapsed:") "time-thunk should print elapsed time"))
  #t)

(run-tests "Debug Utils"
           (list
            test-net-summary-string
            test-dump-links
            test-print-step-sequence
            test-time-thunk))
