(use-modules (icnu utils format)
             (icnu utils assertions)
	     (srfi srfi-1)
             (icnu icnu)
             (icnu rewrite))

(define (test-is-literal-and-get-literal)
  ;; is-literal-node? inspects symbol names heuristically; no need to add nodes.
  (assert-true (is-literal-node? (empty-net) 'lit-true-abc) "lit-true recognized")
  (assert-true (is-literal-node? (empty-net) 'lit-false-xyz) "lit-false recognized")
  (assert-true (is-literal-node? (empty-net) 'num-42) "num- prefix recognized")
  (assert-eq (get-literal-value 'num-42) 42 "get-literal-value numeric")
  (assert-eq (get-literal-value 'lit-true-1) #t "get-literal-value true")
  (assert-eq (get-literal-value 'lit-false-1) #f "get-literal-value false")
  ;; unknown literal form falls back to symbol
  (assert-eq (get-literal-value 'unknown-xyz) 'unknown-xyz "unknown literal falls back to symbol")
  #t)

(define (test-resolve-literal-ep)
  ;; Build a tiny net where a literal node is wired into out.r and ensure resolver follows it.
  (let ((n (parse-net
            '(par
              (node num-1 A)
              (node out A)
              (wire (num-1 p) (out r))))))
    (assert-eq (resolve-literal-ep n (endpoint 'out 'r)) 1 "resolve literal via out.r")
    ;; Also resolving the literal's own principal should yield the value
    (assert-eq (resolve-literal-ep n (endpoint 'num-1 'p)) 1 "resolve literal principal")
    ;; resolver should not erroneously fold when aux side is non-literal
    (let ((m (parse-net '(par (node ltA A) (node v1 V) (wire (v1 p) (ltA l))))))
      (assert-false (rewrite-pass-const-fold! m) "const-fold doesn't fold with non-literal input")))
  #t)

(define (test-rewrite-pass-wire-cleanup)
  ;; A lone Copier (C) with no links should be removed by the cleanup pass.
  (let ((n (parse-net '(par (node c1 C)))))
    (assert-true (member 'c1 (all-nodes-with-agent n 'C)) "c1 exists before cleanup")
    (assert-true (rewrite-pass-wire-cleanup! n) "rewrite-pass-wire-cleanup! returned true")
    (assert-false (member 'c1 (all-nodes-with-agent n 'C)) "c1 removed after cleanup"))
  ;; copier with a peer should not be removed
  (let ((n2 (parse-net '(par (node c2 C) (node a A) (wire (c2 p) (a p))))))
    (assert-false (rewrite-pass-wire-cleanup! n2) "wire-cleanup doesn't remove copier with peer")
    #t))

(define (test-rewrite-pass-const-fold)
  ;; Create a comparator-like node whose name contains "lt" and feed it two numeric literals.
  ;; Expect the comparator to be folded (deleted) by the const-fold pass.
  (let ((n (parse-net
            '(par
	      (node lt1 A)
	      (node num-2 A)
	      (node num-3 A)
	      (node out A)
	      (wire (num-2 p) (lt1 l))
	      (wire (num-3 p) (lt1 r))
	      (wire (lt1 p) (out p))))))
    (assert-true (member 'lt1 (all-nodes-with-agent n 'A)) "lt1 present before const-fold")
    (assert-true (rewrite-pass-const-fold! n) "rewrite-pass-const-fold! returned true")
    (assert-false (member 'lt1 (all-nodes-with-agent n 'A)) "lt1 removed after const-fold"))
  ;; comparator where one side unresolved should not fold
  (let ((n2 (parse-net '(par (node lt2 A) (node num-4 A) (wire (num-4 p) (lt2 l))))))
    (assert-false (rewrite-pass-const-fold! n2) "const-fold doesn't fold when one side unresolved"))
  #t)

(define (test-AC_AE_CE_rules)
  ;; AC: Applicator + Copier where copier.p -> applicator.p causes copier removal
  (let ((n (parse-net '(par (node a A) (node c C) (wire (a p) (c p))))))
    (assert-true (member 'c (all-nodes-with-agent n 'C)) "c exists before AC")
    (assert-true (rewrite-pass-copy-fold! n) "rewrite-pass-copy-fold! applied AC")
    (assert-false (member 'c (all-nodes-with-agent n 'C)) "c removed after AC"))

  ;; AE: Applicator + Eraser should remove both
  (let ((n2 (parse-net '(par (node a2 A) (node e E) (wire (a2 p) (e p))))))
    (assert-true (member 'a2 (all-nodes-with-agent n2 'A)) "a2 exists before AE")
    (assert-true (rewrite-pass-copy-fold! n2) "rewrite-pass-copy-fold! applied AE")
    (assert-false (member 'a2 (all-nodes-with-agent n2 'A)) "a2 removed after AE")
    (assert-false (member 'e (all-nodes-with-agent n2 'E)) "e removed after AE")

    ;; CE: Copier + Eraser should both be removed when connected
    (let ((n3 (parse-net '(par (node c3 C) (node e3 E) (wire (c3 p) (e3 p))))))
      (assert-true (member 'c3 (all-nodes-with-agent n3 'C)) "c3 exists before CE")
      (assert-true (rewrite-pass-copy-fold! n3) "rewrite-pass-copy-fold! applied CE")
      (assert-false (member 'c3 (all-nodes-with-agent n3 'C)) "c3 removed after CE")
      (assert-false (member 'e3 (all-nodes-with-agent n3 'E)) "e3 removed after CE"))
    #t))

(define (test-rewrite-pass-AA-merge)
  ;; AA: merge two applicators connected by p<->p into a single applicator where possible.
  (let ((n (parse-net '(par (node a A) (node b A) (wire (a p) (b p))))))
    (assert-true (member 'a (all-nodes-with-agent n 'A)) "a exists before AA")
    (assert-true (rewrite-pass-AA-merge! n) "rewrite-pass-AA-merge! applied")
    (let ((has-a (member 'a (all-nodes-with-agent n 'A)))
          (has-b (member 'b (all-nodes-with-agent n 'A))))
      ;; After a successful merge, both shouldn't remain as distinct A nodes.
      (assert-true (not (and has-a has-b)) "one of a/b removed after AA"))
    #t))

;; -- Runner ---------------------------------------------------------
(define (run-all-rewrite-tests)
  (let ((tests (list
                test-is-literal-and-get-literal
                test-resolve-literal-ep
                test-rewrite-pass-wire-cleanup
                test-rewrite-pass-const-fold
                test-AC_AE_CE_rules
                test-rewrite-pass-AA-merge)))
    (display "Running rewrite unit tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res
		      (display "ok\n")
		      (display "FAIL\n"))))
	      tests)
    (display "All rewrite tests completed.\n")
    #t))

;; If run as a script, execute the tests.
(run-all-rewrite-tests)
