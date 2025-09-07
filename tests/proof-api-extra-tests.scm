(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu utils internal)
             (icnu icnu)
             (icnu tools icnu-proof)
             (icnu tools icnu-mermaid)
             (icnu utils log)
             (tests test-runner)
             (ice-9 ftw)
             (ice-9 popen))

;; 이 파일은 icnu/tools/icnu-proof.scm의 래퍼(작은/큰 단계, 요약 출력 등)에
;; 대해 누락될 수 있는 경계 동작을 보완하는 추가적인 단위 테스트들을 포함합니다.
;; 권장 실행 커맨드:
;;   guile -L . tests/proof-api-extra-tests.scm
;; 또는 전체 테스트:
;;   make test

(set-debug-level! 0)

(define (test-small-step-no-op)
  "활성 페어가 없는 단순한 넷에 대해 small-step-net이 #f를 반환하는지 확인"
  (let* ((net (parse-net '(par (node a A)))))
    (let ((next (small-step-net (copy-net net))))
      (assert-false next "small-step-net on net with no active pairs should return #f"))
    #t))

(define (test-small-step-string_no_change)
  "small-step-string이 변화가 없을 때 #f를 반환하는지 확인"
  (let ((s "(par (node a A))"))
    (let ((res (small-step-string s)))
      (assert-false res "small-step-string should return #f when no step applicable")
      #t)))

(define (test-run-steps-terminates-when-no-change)
  "run-steps-on-string이 변화가 없는 입력에서 조기 종료하며 #t를 반환하는지 확인"
  (let ((s "(par (node a A))"))
    (let ((ok (run-steps-on-string s 5)))
      (assert-true ok "run-steps-on-string should return #t even when no steps occur")
      #t)))

(define (test-mermaid-helpers)
  (assert-eq (sanitize-id-ml 'a.b/c) "na_b_c" "sanitize-id-ml replaces special chars")
  (assert-eq (sanitize-id-ml "123_ok") "n123_ok" "sanitize-id-ml handles numeric start")
  (assert-eq (escape-ml-label "\"hello\"") "\\\"hello\\\"" "escape-ml-label handles quotes")
  (assert-eq (escape-ml-label "a[b]") "a_b_" "escape-ml-label handles brackets")
  #t)

(define (test-run-steps-on-string->mermaid)
  "run-steps-on-string->mermaid should create files in the specified directory"
  (let* ((s "(par (node a A) (node b A) (wire (a p) (b p)))")
         (out-dir "tests/mermaid-temp-test-output")
         (cmd-rm (string-append "rm -rf " out-dir)))
    (system cmd-rm) ; clean before
    (assert-true (run-steps-on-string->mermaid s 5 out-dir) "run-steps...->mermaid should return #t")
    (let* ((files (scandir out-dir (lambda (x) (not (string-prefix? "." x))))))
      (assert-true (>= (length files) 2) "at least two mermaid files should be created")
      (assert-true (string-suffix? ".mmd" (car files)) "files should have .mmd extension"))
    (system cmd-rm) ; clean after
    #t))

(run-tests "ProofAPI-Extra"
           (list
            test-small-step-no-op
            test-small-step-string_no_change
            test-run-steps-terminates-when-no-change
            test-mermaid-helpers
            test-run-steps-on-string->mermaid))
