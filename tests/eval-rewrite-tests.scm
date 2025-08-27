(use-modules (icnu utils format)
             (icnu utils assertions)
             (srfi srfi-1)
             (icnu icnu)
             (icnu rewrite)
             (icnu utils strings)
             (icnu eval)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

;; -- from tests/rewrite-tests.scm --
(define (test-is-literal-and-get-literal)
  ;; is-literal-node? inspects symbol names heuristically; no need to add nodes.
  (assert-true (is-literal-node? (empty-net) 'lit-true-abc) "lit-true recognized")
  (assert-true (is-literal-node? (empty-net) 'lit-false-xyz) "lit-false recognized")
  (assert-true (is-literal-node? (empty-net) 'num-42) "num- prefix recognized")
  (assert-true (is-literal-node? (empty-net) 'str-hello) "str- prefix recognized")
  (assert-true (is-literal-node? (empty-net) 'trig-num-123) "trig-num- prefix recognized")
  (assert-true (is-literal-node? (empty-net) 'trig-str-abc) "trig-str- prefix recognized")
  (assert-eq (get-literal-value 'num-42) 42 "get-literal-value numeric")
  (assert-eq (get-literal-value 'lit-true-1) #t "get-literal-value true")
  (assert-eq (get-literal-value 'lit-false-1) #f "get-literal-value false")
  (assert-eq (get-literal-value 'str-hello_world) "hello world" "get-literal-value string")
  (assert-eq (get-literal-value 'trig-num-123-xyz) 123 "get-literal-value trig-num")
  (assert-eq (get-literal-value 'trig-str-foo_bar-xyz) "foo bar" "get-literal-value trig-str")
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

(define (test-eraser-creation)
  (let ((n (empty-net)))
    (add-node! n 'e 'E)
    (assert-eq (node-agent n 'e) 'E "Eraser node created with agent E")
    #t))

(define (test-eraser-application)
  (let ((n (empty-net)))
    (add-node! n 'a 'A)
    (add-node! n 'e 'E)
    (link-peers! n (endpoint 'a 'p) (endpoint 'e 'p))
    ;; Apply the AE rule via the copy-fold pass
    (assert-true (rewrite-pass-copy-fold! n) "AE rule applied via copy-fold")
    ;; Both nodes should have been deleted
    (assert-false (node-agent n 'a) "Applicator A removed after AE")
    (assert-false (node-agent n 'e) "Eraser E removed after AE")
    #t))

(define (test-if-fold-non-boolean-condition)
  (let ((net (parse-net
              '(par
                (node if-impl A) (node cond-copy C) (node out-if A)
                (node num-123 A) ; Non-boolean literal
                (wire (num-123 p) (cond-copy p))
                (wire (cond-copy l) (if-impl p))
                (wire (cond-copy r) (out-if p))
                (wire (if-impl l) (then-lit p))
                (node then-lit A)))))
    (assert-false (rewrite-pass-if-fold! net) "if-fold should not apply with non-boolean condition"))
  #t)

(define (test-resolve-literal-ep-cycle)
  (let* ((net (parse-net '(par (node c1 C)
                               (node c2 C)
                               (node out A)
                               (wire (c1 p) (c2 p))
                               (wire (c2 p) (c1 p))
                               (wire (out p) (c1 p)))))
         (result (resolve-literal-ep net (endpoint 'out 'p))))
    (assert-eq result *unresolved* "resolve-literal-ep should not loop on cycles"))
  #t)

(define (test-all-rewrite-rules)
  (let* ((net (parse-net
               '(par
                 ;; AC pattern
                 (node a A) (node c C) (wire (a p) (c p))
                 ;; AE pattern
                 (node a2 A) (node e E) (wire (a2 p) (e p))
                 ;; CE pattern
                 (node c3 C) (node e3 E) (wire (c3 p) (e3 p))
                 ;; AA pattern
                 (node a3 A) (node b3 A) (wire (a3 p) (b3 p))
                 ;; const-fold pattern (lt)
                 (node lt A) (node num-2 A) (node num-3 A)
                 (wire (num-2 p) (lt l)) (wire (num-3 p) (lt r)) (wire (lt p) (out p))
                 ;; if-fold pattern with true condition
                 (node if-impl A) (node cond-copy C) (node out-if A)
                 (node cond-lit A)               ; added literal true node
                 (wire (cond-lit p) (cond-copy p)) ; connect literal to condition copier
                 (wire (cond-copy l) (if-impl p))
                 (wire (cond-copy r) (out-if p))
                 (wire (if-impl l) (then-lit p))
                 (node then-lit A)
                 ;; orphan copier for cleanup
                 (node orphan C))))
         )
    ;; Apply each rewrite pass and assert it reports a change.
    (assert-true (rewrite-pass-copy-fold! net) "copy-fold applied")
    ;; AE already handled by copy-fold pass
    (assert-true (rewrite-pass-AA-merge! net) "AA‑merge applied")
    (assert-true (rewrite-pass-const-fold! net) "const‑fold applied")
    (assert-true (rewrite-pass-if-fold! net) "if‑fold applied")
    (assert-true (rewrite-pass-wire-cleanup! net) "wire‑cleanup applied")
    #t))

;; -- from tests/eval-tests.scm --
(define (eval-from-string s)
  (eval-net (parse-net (read-sexpr-from-string s))))

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

;; -- from tests/rewrite-rule-coverage.scm --
(define (test-rule-AC)
  (let ((net (parse-net '(par (node a A) (node c C) (wire (a p) (c p))))))
    (assert-true (rewrite-pass-copy-fold! net) "AC rule applied via rewrite-pass-copy-fold!")
    (assert-false (member 'c (all-nodes-with-agent net 'C)) "c removed after AC")
    #t))

(define (test-rule-AE)
  (let ((net (parse-net '(par (node a A) (node e E) (wire (a p) (e p))))))
    (assert-true (rewrite-pass-copy-fold! net) "AE rule applied via rewrite-pass-copy-fold!")
    (assert-false (node-agent net 'a) "a removed after AE")
    (assert-false (node-agent net 'e) "e removed after AE")
    #t))

(define (test-rule-CE)
  (let ((net (parse-net '(par (node c C) (node e E) (wire (c p) (e p))))))
    (assert-true (rewrite-pass-copy-fold! net) "CE rule applied via rewrite-pass-copy-fold!")
    (assert-false (member 'c (all-nodes-with-agent net 'C)) "c removed after CE")
    (assert-false (node-agent net 'e) "e removed after CE")
    #t))

(define (test-rule-AA)
  (let ((net (parse-net '(par (node a A) (node b A) (wire (a p) (b p))))))
    (assert-true (rewrite-pass-AA-merge! net) "AA rule applied via rewrite-pass-AA-merge!")
    ;; After merge, both distinct a/b should not remain as separate applicators.
    (let ((has-a (member 'a (all-nodes-with-agent net 'A)))
          (has-b (member 'b (all-nodes-with-agent net 'A))))
      (assert-true (not (and has-a has-b)) "one of a/b removed after AA"))
    #t))

(define (test-rule-const-fold)
  (let ((net (parse-net
              '(par
                (node lt1 A)
                (node num-2 A)
                (node num-3 A)
                (node out A)
                (wire (num-2 p) (lt1 l))
                (wire (num-3 p) (lt1 r))
                (wire (lt1 p) (out p))))))
    (assert-true (rewrite-pass-const-fold! net) "const-fold applied")
    (assert-false (member 'lt1 (all-nodes-with-agent net 'A)) "lt1 removed after const-fold")
    #t))

(define (test-rule-if-fold)
  ;; Build small if-impl with a literal true condition to trigger if-fold
  (let ((net (parse-net
              '(par
                (node if-impl A)
                (node cond-copy C)
                (node lit-true-1 A)
                (node then-lit A)
                (node else-lit A)
                (wire (lit-true-1 p) (cond-copy p))
                (wire (cond-copy l) (if-impl p))
                (wire (if-impl l) (then-lit p))
                (wire (if-impl r) (else-lit p))))))
    (assert-true (rewrite-pass-if-fold! net) "if-fold applied")
    ;; if-impl should be removed after folding
    (assert-false (member 'if-impl (all-nodes-with-agent net 'A)) "if-impl removed after if-fold")
    #t))

(define (test-rule-wire-cleanup)
  (let ((net (parse-net '(par (node c1 C)))))
    (assert-true (rewrite-pass-wire-cleanup! net) "wire-cleanup removed orphan copier")
    (assert-false (member 'c1 (all-nodes-with-agent net 'C)) "c1 removed after cleanup")
    #t))


(run-tests "Eval-Rewrite"
           (list
            test-is-literal-and-get-literal
            test-resolve-literal-ep
            test-rewrite-pass-wire-cleanup
            test-rewrite-pass-const-fold
            test-AC_AE_CE_rules
            test-rewrite-pass-AA-merge
            test-eraser-creation
            test-eraser-application
            test-all-rewrite-rules
            test-if-fold-non-boolean-condition
            test-resolve-literal-ep-cycle
            test-eval-aa-merge-to-string
            test-eval-const-fold
            test-reduce-nu-scope
            test-reduce-eraser
            test-eval-if-true
            test-rule-AC
            test-rule-AE
            test-rule-CE
            test-rule-AA
            test-rule-const-fold
            test-rule-if-fold
            test-rule-wire-cleanup))
