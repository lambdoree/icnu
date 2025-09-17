(use-modules (ice-9 rdelim)
             (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu stdlib icnu-lib)
             (icnu stdlib unit)
             (icnu utils log))

(set-debug-level! 0)

;; IC_UNIT + IC_Y 혼합 예제:
;; - 유닛 내부에서 Y-콤비네이터 네트를 구성하지만, 결과는 상수 42를 반환하도록 함.
;; - IC_CALL_UNIT로 유닛을 호출해 result 포트에서 값을 관찰.

(define forty-two-body
  `(par
    ;; ICNU_Y를 포함하되, 실제 계산은 입력 n에 대해 1..n 합을 구함.
    (node yfn A y-fn)
    ,(ICNU_Y 'yfn 'y-out)
    ,(ICNU_PRIM_SUM1 '(in-pack l) 'ret)))

(define forty-two-unit-sexpr
  (IC_UNIT 'forty-two '() '() 'ret forty-two-body))

(define call-sexpr
  (append
    (list
     (ICNU_LITERAL 7 'n7)             ; n = 7
     (ICNU_NIL 'out-nil)
     '(node result C))
    (IC_CALL_UNIT 'forty-two '(n7 p) '(out-nil p) 'result)))

(define combined-sexpr
  `(par ,@forty-two-unit-sexpr ,@call-sexpr))

(format-string #t "==== Demo: IC_UNIT + IC_Y ===~%~%")

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
