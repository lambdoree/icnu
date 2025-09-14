(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu stdlib ic-lib)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

(define (eval-sexpr sexpr out-name)
  (eval-net (parse-net sexpr #f) `((out-name . ,out-name))))

(define (test-IC_PURE_ID)
  (let* ((v 123)
         (sexpr `(par ,@(IC_LITERAL v 'in)
                      ,@(IC_PURE_ID 'in 'out))))
    (assert-eq (eval-sexpr sexpr 'out) v "IC_PURE_ID should return its input value"))
  #t)

(define (test-IC_PURE_PAIR_FST_SND)
  (let* ((v1 10) (v2 "v2")
         (pair-sexpr-body `(,@(IC_LITERAL v1 'in1)
                            ,@(IC_LITERAL v2 'in2)
                            ,@(IC_PURE_PAIR 'in1 'in2 'p-out))))
    (let* ((fst-sexpr `(par ,@pair-sexpr-body
                           ,@(IC_PURE_FST '(p-out p) 'fst-out)))
           (snd-sexpr `(par ,@pair-sexpr-body
                           ,@(IC_PURE_SND '(p-out p) 'snd-out))))
      (assert-eq (eval-sexpr fst-sexpr 'fst-out) v1 "IC_PURE_FST should extract the first element")
      (assert-eq (eval-sexpr snd-sexpr 'snd-out) v2 "IC_PURE_SND should extract the second element")))
  #t)

(define (test-IC_PURE_constructors_are_valid_nets)
  (assert-true (parse-net (IC_PURE_LEFT 'a 'out) #f) "IC_PURE_LEFT is a valid net")
  (assert-true (parse-net (IC_PURE_RIGHT 'b 'out) #f) "IC_PURE_RIGHT is a valid net")
  (assert-true (parse-net (IC_PURE_EITHER '(v p) 'l 'r 'out) #f) "IC_PURE_EITHER is a valid net")
  #t)

(run-tests "StdLib-IC-Pure"
           (list
            test-IC_PURE_ID
            test-IC_PURE_PAIR_FST_SND
            test-IC_PURE_constructors_are_valid_nets))
