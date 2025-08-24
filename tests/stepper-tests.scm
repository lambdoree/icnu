(use-modules (icnu utils format)
             (icnu utils strings)
             (icnu utils assertions)
             (icnu stepper)
             (icnu icnu))

(define (test-reduce-once)
  (let* ((input "(par (node a A) (node b A) (wire (a p) (b p)))")
         (result-str (reduce-once input))
         (result-net (call-with-input-string result-str (lambda (p) (parse-net (read p))))))
    (assert-eq (length (all-nodes-with-agent result-net 'A)) 1 "reduce-once results in one A-node")
    (assert-false (node-agent result-net 'a) "reduce-once removes original node 'a'")
    (assert-false (node-agent result-net 'b) "reduce-once removes original node 'b'"))
  #t)

(define (test-reduce-to-normal-form)
  (let* ((input "(par (node lt1 A) (node c C) (wire (num-2 p) (c p)) (wire (c l) (lt1 l)) (wire (num-3 p) (lt1 r)) (wire (lt1 p) (out p)) (node num-2 A) (node num-3 A) (node out A))")
         (result (reduce-to-normal-form input))
         (final-net (call-with-input-string result (lambda (p) (parse-net (read p))))))
    (assert-true (string-contains? result "lit-true") "Normal form contains true literal")
    (assert-false (node-agent final-net 'lt1) "Normal form does not contain original comparator node"))
  #t)

(define (test-reduce-nu-scope)
  (let* ((input "(nu (x y) (par (node x A) (node y A) (wire (x p) (y p))))")
         (result (reduce-once input)))
    (assert-true (string-contains? result "(nu (a-") "reduce-once preserves nu scope for new nodes")
    (assert-false (string-contains? result " x ") "reduce-once with nu removes original node 'x'")
    (assert-false (string-contains? result " y ") "reduce-once with nu removes original node 'y'"))
  #t)

(define (test-reduce-eraser)
  (let* ((input "(nu (a e) (par (node a A) (node e E) (wire (a p) (e p))))")
         (result (reduce-to-normal-form input)))
    (assert-true (string-contains? result "nu") "Eraser reduction preserves nu scope")
    (assert-false (string-contains? result "node") "Eraser reduction removes all nodes"))
  #t)

(define (test-reduce-if-true)
  (let* ((input "(par
                   (node if-impl A) (node cond-copy C) (node then-copy C) (node else-copy C)
                   (node true A) (node then-val A) (node else-val A) (node out A)
                   (wire (true p) (cond-copy p))
                   (wire (cond-copy l) (if-impl p))
                   (wire (then-val p) (then-copy p))
                   (wire (then-copy l) (if-impl l))
                   (wire (else-val p) (else-copy p))
                   (wire (else-copy l) (if-impl r))
                   (wire (if-impl p) (out p)))")
         (result (reduce-to-normal-form input))
         (final-net (call-with-input-string result (lambda (p) (parse-net (read p))))))
    (assert-false (node-agent final-net 'if-impl) "IF construct is removed")
    (assert-false (node-agent final-net 'else-copy) "Pruned branch copier is removed")
    #t))

(define (run-all-stepper-tests)
  (let ((tests (list
                test-reduce-once
                test-reduce-to-normal-form
                test-reduce-nu-scope
                test-reduce-eraser
                test-reduce-if-true)))
    (display "Running stepper tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res
		      (display "ok\n")
		      (display "FAIL\n"))))
	      tests)
    (display "All stepper tests completed.\n")
    #t))

(run-all-stepper-tests)
