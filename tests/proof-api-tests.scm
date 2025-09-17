(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu utils internal)
             (icnu utils strings)
             (icnu utils log)
             (icnu icnu)
             (icnu eval)
             (icnu stdlib icnu-lib)
             (icnu tools icnu-proof)
             (tests test-runner))

(set-debug-level! 0)

(define test-str
  "(nu () (par (node n2 A lit/num 2) (node n4 A lit/num 4) (node s-lt A lit/str lt) (node s-gt A lit/str ge) (node s-eqL A lit/str lt-eq-left) (node add A prim/add) (node lt1 A prim/lt) (node if1 A prim/if) (node cc1 C) (node lt2 A prim/lt) (node if2 A prim/if) (node cc2 C) (node c2 C) (node c4a C) (node c4b C) (node out A) (wire (c2 p) (n2 p)) (wire (add l) (c2 l)) (wire (add r) (c2 r)) (wire (lt1 l) (add p)) (wire (c4a p) (n4 p)) (wire (lt1 r) (c4a l)) (wire (c4b p) (c4a r)) (wire (lt2 l) (c4b l)) (wire (lt2 r) (c4b r)) (wire (cc1 p) (lt1 p)) (wire (if1 l) (s-lt p)) (wire (if1 r) (if2 p)) (wire (cc1 l) (if1 p)) (wire (cc1 r) (out p)) (wire (cc2 p) (lt2 p)) (wire (if2 l) (s-eqL p)) (wire (if2 r) (s-gt p))))"
  )


(define (test-aa-step-sequence)
  (let* ((s "(par (node a A) (node b A) (wire (a p) (b p)))")
         (sexpr (read-sexpr-from-string s))
         (net   (parse-net sexpr))
         (seq (let loop ((cur (copy-net net)) (acc '()) (i 0))
                (let ((next (small-step-net (copy-net cur))))
                  (if (or (not next) (>= i 10))
                      (reverse (cons (format-string #f "~a" (pretty-print cur '((show-nu? . #t)))) acc))
                      (loop next (cons (format-string #f "~a" (pretty-print cur '((show-nu? . #t)))) acc) (+ i 1)))))))
    (assert-true (>= (length seq) 2) "AA sequence has at least 2 steps")
    (let ((last (car (reverse seq))))
      (assert-true (string-contains? last "a-0") "final step contains merged node name a-0"))
    #t))

(define (test-const-fold-step)
  (let* ((s "(par (node lt1 A 'prim/lt) (node n2 A 'lit/num 2) (node n3 A 'lit/num 3) (node out A) (wire (n2 p) (lt1 l)) (wire (n3 p) (lt1 r)) (wire (lt1 p) (out p)))")
         (res (small-step-string s)))
    (assert-true res "small-step-string returned something for const-fold")
    (assert-true (or (string-contains? res "#t") (string-contains? res "lit/bool")) "const-fold produced a boolean literal")
    #t))

(define (test-if-fold-step)
  (let* ((s "(par (node if-impl A 'prim/if) (node cond-copy C) (node true A 'lit/bool #t) (node then-val A 'lit/str 'then) (node else-val A 'lit/str 'else) (node out A) (wire (true p) (cond-copy p)) (wire (cond-copy l) (if-impl p)) (wire (then-val p) (if-impl l)) (wire (else-val p) (if-impl r)) (wire (cond-copy r) (out p)))")
         (big (big-step-string s)))
    (assert-true (string-contains? big "then") "big-step selected then branch (output contains 'then')")
    #t))

(define (test-church-small-vs-big)
  (let* ((sexpr (ICNU_CHURCH-APPLY 3 'f 'x 'outc))
         (net (parse-net sexpr))
         (reduced (big-step-net (copy-net net) '((max-iter . 200)))))
    (assert-true (node-agent reduced 'outc) "big-step result has outc node")
    (let* ((seq (small-step-sequence-net net 200))
           (last-net (car (reverse seq))))
      (assert-true (node-agent last-net 'outc) "small-step final net has outc node"))
    #t))

(define (test-small-vs-big-agreement)
  (let* ((s "(par (node a A) (node b A) (wire (a p) (b p)))")
         (net (parse-net (read-sexpr-from-string s)))
         (big (big-step-net (copy-net net) '((max-iter . 50))))
         (seq (small-step-sequence-net net 50))
         (last (car (reverse seq))))
    (assert-true (or (icnu-any (lambda (nm) (string-prefix? "a-0" (symbol->string nm))) (all-names big))
                     (icnu-any (lambda (nm) (string-prefix? "a-0" (symbol->string nm))) (all-names last)))
                 "either big-step or small-step produced a merged node with prefix a-0")
    #t))


(define (test-small-step-string-basic)
  (let ((s "(par (node a A) (node b A) (wire (a p) (b p)))"))
    (let ((res (small-step-string s)))
      (assert-true res "small-step-string returned something")
      #t)))

(define (test-big-step-string-basic)
  (let ((s "(par (node a A) (node b A) (wire (a p) (b p)))"))
    (let ((res (big-step-string s)))
      (assert-true res "big-step-string returned something")
      #t)))

(define (test-small-step-net-applies-AE)
  (let* ((net (parse-net '(par (node a A) (node e E) (wire (a p) (e p)))))
         (next (small-step-net net)))
    (assert-true next "small-step-net returned a net")
    (assert-false (node-agent next 'a) "node 'a' removed by AE")
    (assert-false (node-agent next 'e) "node 'e' removed by AE")
    #t))

(define (test-run-steps-print)
  (let ((s test-str))
    (let ((ok (run-steps-on-string s 10)))
      (assert-true ok "run-steps-on-string returned #t")
      #t)))

(run-tests "ProofAPI"
           (list
            test-small-step-string-basic
            test-big-step-string-basic
            test-small-step-net-applies-AE
            test-run-steps-print
			))
