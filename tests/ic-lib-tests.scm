(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu ic)
             (icnu eval)
             (icnu stdlib ic-lib)
             (icnu tools icnu-validate)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

(define (test-IC_PURE_constructors_are_valid_nets)
  (assert-true (parse-net (IC_PURE_LEFT 'a 'out) #f) "IC_PURE_LEFT is a valid net")
  (assert-true (parse-net (IC_PURE_RIGHT 'b 'out) #f) "IC_PURE_RIGHT is a valid net")
  (assert-true (parse-net (IC_PURE_EITHER '(v p) 'l 'r 'out) #f) "IC_PURE_EITHER is a valid net")
  #t)

(run-tests "StdLib-IC-Pure"
           (list
            test-IC_PURE_constructors_are_valid_nets))

(define (test-IC_PRIM_ADD_const_fold)
  (let* ((sexpr `(par
                  (node a A lit/num 2)
                  (node b A lit/num 3)
                  (node out A)
                  ,@(IC_PRIM_ADD 'a 'b 'out)))
         (net (parse-net sexpr #f))
         (res (eval-net net '((out-name . out)))))
    (assert-eq res 5 "IC_PRIM_ADD 2 + 3 => 5")))

(define (test-IC_PRIM_ADD_RUNTIME_const_fold)
  (let* ((sexpr `(par
                  (node a A lit/num 10)
                  (node b A lit/num 32)
                  (node out A)
                  ,@(IC_PRIM_ADD_RUNTIME 'a 'b 'out)))
         (net (parse-net sexpr #f))
         (res (eval-net net '((out-name . out)))))
    (assert-eq res 42 "IC_PRIM_ADD_RUNTIME 10 + 32 => 42")))

(define (test-IC_IF_const_true)
  (let* ((sexpr `(par
                  (node c A lit/bool #t)
                  (node t A lit/num 7)
                  (node e A lit/num 9)
                  (node out A)
                  ,@(IC_IF 'c 't 'e 'out)))
         (net (parse-net sexpr #f))
         (res (eval-net net '((out-name . out)))))
    (assert-eq res 7 "IC_IF #t chooses then-branch")))

(define (test-IC_APPLY_is_valid_net)
  (let* ((sexpr `(par
                  (node f A)
                  (node x A)
                  ,@(IC_APPLY 'f 'x 'out)))
         (net (parse-net sexpr #f)))
    (assert-true (null? (validate-ir net)) "IC_APPLY produces a valid net")
    #t))

(define (test-IC_COPY_is_valid_net)
  (let* ((sexpr `(par
                  (node in A)
                  ,@(IC_COPY 'in 'out)))
         (net (parse-net sexpr #f)))
    (assert-true (null? (validate-ir net)) "IC_COPY produces a valid net")
    #t))

(define (test-IC_CONS_FIRST_REST_valid)
  (let* ((sexpr `(par
                  (node h A lit/num 1)
                  ,@(IC_NIL 'nil)
                  ,@(IC_CONS '(h p) 'nil 'cons)
                  ,@(IC_FIRST 'cons 'first)
                  ,@(IC_REST 'cons 'rest)))
         (net (parse-net sexpr #f)))
    (assert-true (null? (validate-ir net)) "IC_CONS/IC_FIRST/IC_REST shape is valid")
    #t))

(define (test-IC_CHURCH_APPLY_n0_valid)
  (let* ((sexpr `(par
                  (node f A)
                  (node x A)
                  ,@(IC_CHURCH-APPLY 0 'f 'x 'out0)))
         (net (parse-net sexpr #f)))
    (assert-true (null? (validate-ir net)) "IC_CHURCH-APPLY n=0 valid")
    #t))

(define (test-IC_CHURCH_APPLY_n2_valid)
  (let* ((sexpr `(par
                  (node f A)
                  (node x A)
                  ,@(IC_CHURCH-APPLY 2 'f 'x 'out2)))
         (net (parse-net sexpr #f)))
    (assert-true (null? (validate-ir net)) "IC_CHURCH-APPLY n=2 valid")
    #t))

(define (test-JOIN_ops_valid)
  (let* ((sexpr1 `(par (node x A) (node y A) (node out A) ,@(JOIN-REPLACE 'x 'y 'out)))
         (sexpr2 `(par (node x A) (node y A) (node out A) ,@(JOIN-MAX 'x 'y 'out)))
         (net1 (parse-net sexpr1 #f))
         (net2 (parse-net sexpr2 #f)))
    (assert-true (null? (validate-ir net1)) "JOIN-REPLACE valid")
    (assert-true (null? (validate-ir net2)) "JOIN-MAX valid")
    #t))

(run-tests "IC-Lib"
           (list
            test-IC_PRIM_ADD_const_fold
            test-IC_PRIM_ADD_RUNTIME_const_fold
            test-IC_IF_const_true
            test-IC_APPLY_is_valid_net
            test-IC_COPY_is_valid_net
            test-IC_CONS_FIRST_REST_valid
            test-IC_CHURCH_APPLY_n0_valid
            test-IC_CHURCH_APPLY_n2_valid
            test-JOIN_ops_valid))
