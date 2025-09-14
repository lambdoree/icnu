(use-modules (ice-9 rdelim)
             (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu stdlib ic-lib)
             (icnu stdlib unit)
             (icnu utils log))

(set-debug-level! 0)

(define add-one-body
  `(par
    ,@(IC_LITERAL 1 'one)
    ,@(IC_PRIM_ADD '(in-pack l) '(one p) 'ret)
    ))

(define add-one-unit-sexpr
  (IC_UNIT 'add-one '() '() 'ret add-one-body))

(define call-sexpr
  (append
    (IC_LITERAL 7 'v7)
    (IC_NIL 'nil2)
    (list '(node result C))
    (IC_CALL_UNIT 'add-one '(v7 p) '(nil2 p) 'result)))

(define combined-sexpr
  `(par ,@add-one-unit-sexpr ,@call-sexpr))

(format-string #t "==== Demo: IC_UNIT + IC_CALL_UNIT ===~%~%")

(format-string #t "---- Raw IC S-expression (expanded macros) ----~%~a~%~%"
               combined-sexpr)

(define net (parse-net combined-sexpr))

(format-string #t "---- Pretty-printed parsed net ----~%~a~%~%"
               (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))

(define reduced (reduce-net-to-normal-form (copy-net net) '((max-iter . 200))))

(format-string #t "---- Pretty-printed reduced net (normal form) ----~%~a~%~%"
               (format-string #f "~a" (pretty-print reduced '((show-nu? . #t)))))

(define observed (eval-net (copy-net net) '((out-name . result))))

(format-string #t "---- Observed result via eval-net (out-name = result) ----~%~a~%~%"
               observed)

(format-string #t "Demo complete.~%")
