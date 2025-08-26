(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu eval)
             (icnu tools icnu-validate))

(define (safe-parse sexpr)
  (catch 'safe-parse
    (lambda () (parse-net sexpr) #t)
    (lambda args #f)))

(define (test-parse-net-restores-link-mode)
  (let ((orig (*link-conflict-mode*)))
    (set-link-conflict-mode! 'error)
    (let ((sexpr '(par (node a A) (node b A) (node c A)
                       (wire (a p) (b p))
                       (wire (a p) (c p)))))
      (let ((ok (safe-parse sexpr)))
        (assert-true ok "parse-net should not raise when parsing potentially conflicting surface form")))
    (assert-eq (*link-conflict-mode*) 'error "parse-net should restore global link-conflict-mode")
    (set-link-conflict-mode! orig)
    #t))

(define (test-reduce-respects-max-iter)
  (let ((base-sexpr '(par (node a A) (node b A) (node c C)
                          (wire (a p) (c p))
                          (wire (c l) (b p)))))
    (let ((net1 (parse-net base-sexpr)))
      (let ((res (reduce-net-to-normal-form net1 '((max-iter . 0)))))
        (assert-true res "reduce with max-iter 0 should return without error")))
    (let ((net2 (parse-net base-sexpr)))
      (let ((res2 (reduce-net-to-normal-form net2 '((max-iter . 5)))))
        (assert-true res2 "reduce with max-iter 5 should return without error")))
    #t))

(define (test-format-string-accepts-port)
  (let ((out (call-with-output-string (lambda (p) (format-string p "hello ~a" "world")))))
    (assert-eq out "hello world" "format-string should write to provided output port"))
  (let ((out2 (call-with-output-string (lambda (p) (parameterize ((current-output-port p)) (format-string #t "x~a" 1))))))
    (assert-eq out2 "x1" "format-string #t prints to current-output-port"))
  #t)

(define (test-IC_COPY-self-name)
  (let ((sexpr (IC_COPY 'x 'x)))
    (let ((net (parse-net sexpr)))
      (assert-true (node-agent net 'x) "IC_COPY created or preserved node x")
      #t)))

(define (test-validate-ir-bad-link-format)
  (let ((n (empty-net)))
    (hash-set! (net-links n) 'bad-key (endpoint 'a 'p))
    (let ((errs (validate-ir n)))
      (assert-true (not (null? errs)) "validate-ir should report bad link format"))
    #t))

(define (test-resolve-literal-trigger-style)
  (let* ((sexpr (IC_LITERAL 7 'trig-out))
         (net (parse-net sexpr)))
    (let ((val (resolve-literal-ep net (endpoint 'trig-out 'r))))
      (assert-true (or (equal? val 7) (equal? val 'num-7)) "trigger-style literal resolves to 7"))
    #t))

(define (run-all-more-edge-tests)
  (let ((tests (list
		test-parse-net-restores-link-mode
		test-reduce-respects-max-iter
		test-format-string-accepts-port
		test-IC_COPY-self-name
		test-validate-ir-bad-link-format
		test-resolve-literal-trigger-style)))
    (display "Running more edge tests...\n")
    (for-each (lambda (t)
		(format-string #t " - ~a ... " (format-string #f "~a" t))
		(let ((res (catch 'more-edge-tests
			     (lambda () (t) #t)
			     (lambda args (display "EXN\n") #f))))
		  (if res
		      (display "ok\n")
		      (display "FAIL\n"))))
	      tests)
    (display "All more edge tests completed.\n")
    #t))

(run-all-more-edge-tests)
