(use-modules (icnu utils format)
             (icnu utils strings)
             (icnu utils assertions)
             (icnu eval)
             (icnu icnu))

(define (read-sexpr-from-string s)
  (call-with-input-string s read))

(define (test-eval-aa-merge-to-string)
  (let* ((input "(par (node a A) (node b A) (wire (a p) (b p)))")
         (net (parse-net (read-sexpr-from-string input)))
         (result-str (eval-net net)))
    (assert-false (string-contains? result-str " a ") "eval removes original node 'a'")
    (assert-false (string-contains? result-str " b ") "eval removes original node 'b'")
    (assert-true (string-contains? result-str " a-") "eval creates a new merged node"))
  #t)

(define (test-eval-const-fold)
  (let* ((input "(par (node lt1 A) (node c C) (wire (num-2 p) (c p)) (wire (c l) (lt1 l)) (wire (num-3 p) (lt1 r)) (wire (lt1 p) (out p)) (node num-2 A) (node num-3 A) (node out A))")
         (net (parse-net (read-sexpr-from-string input)))
         (result (eval-net net '((out-name . out)))))
    (assert-eq result #t "const-fold of 2 < 3 evaluates to #t"))
  #t)

(define (test-reduce-nu-scope)
  (let* ((input "(nu (x y) (par (node x A) (node y A) (wire (x p) (y p))))")
         (net (parse-net (read-sexpr-from-string input)))
         (result (eval-net net)))
    (assert-true (string-contains? result "(nu (a-") "eval preserves nu scope for new nodes")
    (assert-false (string-contains? result " x ") "eval with nu removes original node 'x'")
    (assert-false (string-contains? result " y ") "eval with nu removes original node 'y'"))
  #t)

(define (test-reduce-eraser)
  (let* ((input "(nu (a e) (par (node a A) (node e E) (wire (a p) (e p))))")
         (net (parse-net (read-sexpr-from-string input)))
         (result (eval-net net)))
    (assert-true (string-contains? result "nu") "Eraser reduction preserves nu scope")
    (assert-false (string-contains? result "node") "Eraser reduction removes all nodes"))
  #t)

(define (test-eval-if-true)
  (let* ((input "(par
                   (node if-impl A) (node cond-copy C) (node then-copy C) (node else-copy C)
                   (node true A) (node then-val A) (node else-val A) (node out A)
                   (wire (true p) (cond-copy p))
                   (wire (cond-copy l) (if-impl p))
                   (wire (then-val p) (then-copy p))
                   (wire (then-copy l) (if-impl l))
                   (wire (else-val p) (else-copy p))
                   (wire (else-copy l) (if-impl r))
                   (wire (cond-copy r) (out p)))")
         (net (parse-net (read-sexpr-from-string input)))
         (result (eval-net net '((out-name . out)))))
    (assert-eq result 'then-val "eval of IF selects the 'then' branch value"))
  #t)

(define (run-all-eval-tests)
  (let ((tests (list
		test-eval-aa-merge-to-string
		test-eval-const-fold
		test-reduce-nu-scope
		test-reduce-eraser
		test-eval-if-true)))
    (display "Running eval tests...\n")
    (for-each (lambda (t)
		(format-string #t " - ~a ... " (format-string #f "~a" t))
		(let ((res (t)))
                  (if res
		      (display "ok\n")
		      (display "FAIL\n"))))
	      tests)
    (display "All eval tests completed.\n")
    #t))

(run-all-eval-tests)
