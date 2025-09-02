(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu utils internal)
             (icnu icnu)
             (icnu tools icnu-proof)
             (icnu utils log)
             (tests test-runner))

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

(run-tests "ProofAPI-Extra"
           (list
            test-small-step-no-op
            test-small-step-string_no_change
            test-run-steps-terminates-when-no-change))
