(use-modules (ice-9 rdelim)
             (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu stdlib icnu-lib)
             (icnu stdlib unit)
             (icnu utils log))

(set-debug-level! 0)

(define add-one-body
  `(par
    (wire (in-pack l) (ret p))
    ))

(define add-one-unit-sexpr
  (IC_UNIT 'add-one '() '() 'ret add-one-body))

(define call-sexpr
  `(par
    (node v7 A)
    ,(ICNU_NIL 'nil2)
    (node result C)
    ,@(IC_CALL_UNIT 'add-one '(v7 p) '(nil2 p) 'result)))

(define combined-sexpr
  `(par ,@add-one-unit-sexpr ,call-sexpr))

(format-string #t "==== Demo: IC_UNIT + IC_CALL_UNIT ===~%~%")

(format-string #t "---- Raw IC S-expression (expanded macros) ----~%~a~%~%"
               combined-sexpr)

(define net (parse-net combined-sexpr))

(format-string #t "---- Pretty-printed parsed net ----~%~a~%~%"
               (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))

(define reduced (reduce-net-to-normal-form (copy-net net) '((max-iter . 200) (passes . ic-only-reduction-passes))))

(format-string #t "---- Pretty-printed reduced net (normal form) ----~%~a~%~%"
               (format-string #f "~a" (pretty-print reduced '((show-nu? . #t)))))

(let ((node-cnt (length (all-names reduced)))
      (lit-present?
       (let ((found #f))
         (hash-for-each
          (lambda (nm tag)
            (when (memq tag '(lit/bool lit/num lit/str lit/pair))
              (set! found #t)))
          (net-tags reduced))
         found)))
  (format-string #t "요약: 노드수=~a, lit/* 포함? ~a~%~%" node-cnt lit-present?))

(letrec ((starts-with?
          (lambda (s prefix)
            (let ((ls (string-length s)) (lp (string-length prefix)))
              (and (>= ls lp) (string=? (substring s 0 lp) prefix)))))
         (scm-filter
          (lambda (pred lst)
            (let loop ((xs lst) (acc '()))
              (if (null? xs) (reverse acc)
                  (loop (cdr xs) (if (pred (car xs)) (cons (car xs) acc) acc))))))
         (pick-first
          (lambda (names prefix)
            (let loop ((xs names))
              (if (null? xs) #f
                  (let* ((nm (car xs)) (s (symbol->string nm)))
                    (if (starts-with? s prefix) nm (loop (cdr xs))))))))
         (pp-ep (lambda (ep) (if ep (format-string #f "~a" ep) "none"))))
  (let* ((names (all-names reduced))
         (ret      (pick-first names "ret-"))
         (in-pack  (pick-first names "in-pack-"))
         (out-pack (pick-first names "out-pack-")))
    (format-string #t "해석: add-one: result.p<-~a, add-one.l peer=~a, add-one.r peer=~a~%~%"
                   (pp-ep (peer reduced (endpoint 'result 'p)))
                   (pp-ep (peer reduced (endpoint 'add-one 'l)))
                   (pp-ep (peer reduced (endpoint 'add-one 'r))))
    (format-string #t "  v7.p peer=~a, nil2.p peer=~a~%"
                   (pp-ep (peer reduced (endpoint 'v7 'p)))
                   (pp-ep (peer reduced (endpoint 'nil2 'p))))
    (when ret
      (format-string #t "  내부 ret(~a).l<-~a, ret.p->~a~%"
                     ret
                     (pp-ep (peer reduced (endpoint ret 'l)))
                     (pp-ep (peer reduced (endpoint ret 'p)))))
    (when in-pack
      (format-string #t "  내부 in-pack(~a).l<-~a, in-pack.p->~a~%"
                     in-pack
                     (pp-ep (peer reduced (endpoint in-pack 'l)))
                     (pp-ep (peer reduced (endpoint in-pack 'p)))))
    (when out-pack
      (format-string #t "  내부 out-pack(~a).p->~a~%"
                     out-pack
                     (pp-ep (peer reduced (endpoint out-pack 'p)))))))

(begin
  (define (check-true c msg) (if c #t (error msg)))
  (define (starts-with? s prefix)
    (let ((ls (string-length s)) (lp (string-length prefix)))
      (and (>= ls lp) (string=? (substring s 0 lp) prefix))))
  (let* ((res-peer (peer reduced (endpoint 'result 'p)))
         (expected (endpoint 'add-one 'l)))
    (check-true (equal? res-peer expected)
                "검증 실패: result.p는 add-one.l과 연결되어야 합니다."))
  (letrec ((pick-first
            (lambda (names prefix)
              (let loop ((xs names))
                (if (null? xs) #f
                    (let* ((nm (car xs)) (s (symbol->string nm)))
                      (if (starts-with? s prefix) nm (loop (cdr xs)))))))))
    (let* ((names (all-names reduced))
           (ret      (pick-first names "ret-"))
           (in-pack  (pick-first names "in-pack-"))
           (out-pack (pick-first names "out-pack-")))
      (check-true ret "검증 실패: ret-* 노드를 찾지 못했습니다.")
      (check-true in-pack "검증 실패: in-pack-* 노드를 찾지 못했습니다.")
      (check-true out-pack "검증 실패: out-pack-* 노드를 찾지 못했습니다.")
      ;; 3) 내부 연결 규약 검사
      (check-true (equal? (peer reduced (endpoint ret 'l))
                          (endpoint 'add-one 'p))
                  "검증 실패: ret.l은 add-one.p와 연결되어야 합니다.")
      (check-true (equal? (peer reduced (endpoint ret 'p))
                          (endpoint in-pack 'l))
                  "검증 실패: ret.p는 in-pack.l과 연결되어야 합니다.")
      (check-true (equal? (peer reduced (endpoint in-pack 'p))
                          (endpoint 'v7 'l))
                  "검증 실패: in-pack.p는 v7.l과 연결되어야 합니다.")
      (check-true (equal? (peer reduced (endpoint out-pack 'p))
                          (endpoint 'nil2 'l))
                  "검증 실패: out-pack.p는 nil2.l과 연결되어야 합니다.")))
  (let ((lit-present?
         (let ((found #f))
           (hash-for-each
            (lambda (nm tag)
              (when (memq tag '(lit/bool lit/num lit/str lit/pair))
                (set! found #t)))
            (net-tags reduced))
           found))
        (prim-present?
         (let ((found #f))
           (hash-for-each
            (lambda (nm tag)
              (when (and (symbol? tag)
                         (starts-with? (symbol->string tag) "prim/"))
                (set! found #t)))
            (net-tags reduced))
           found)))
    (check-true (not lit-present?) "검증 실패: lit/* 노드가 발견되었습니다(데이터/계산 분리 위반 가능).")
    (check-true (not prim-present?) "검증 실패: prim/* 노드가 발견되었습니다(순수 IC 계산 위반).")))
(format-string #t "모든 구조 검증 통과: IC_UNIT + IC_CALL_UNIT 순수 IC 배선/축약 OK.~%")
;; Pure-IC mode: skip eval-net (no Scheme-level observation)
(format-string #t "Demo complete.~%")
