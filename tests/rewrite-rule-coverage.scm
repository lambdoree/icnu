(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu rewrite)
             (icnu eval))

;; 목적: 각 리라이터 규칙(AC, AE, CE, AA, const-fold, if-fold, wire-cleanup)이
;; 테스트 스위트 실행 중 최소 한 번은 적용되는지 확인하는 간단한 커버리지 테스트들.
;; 파일 상단의 주석은 설명용이며 테스트 러너에 의해 무시됩니다.

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

(define (run-all-rewrite-rule-coverage-tests)
  (let ((tests (list
                test-rule-AC
                test-rule-AE
                test-rule-CE
                test-rule-AA
                test-rule-const-fold
                test-rule-if-fold
                test-rule-wire-cleanup)))
    (display "Running rewrite rule coverage tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (catch 'rewrite-rule-coverage
                             (lambda () (t) #t)
                             (lambda args (display "EXN\n") #f))))
                  (if res
                      (display "ok\n")
                      (display "FAIL\n"))))
              tests)
    (display "All rewrite rule coverage tests completed.\n")
    #t))

(run-all-rewrite-rule-coverage-tests)

;; To run full test suite: make test
