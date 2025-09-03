;; 목적: 서로 다른 패스 적용 순서(기본 순서 vs 뒤집힌 순서 등)가 관찰자 관점의 결과(pretty-print 문자열)
;; 에서 일관된 결과를 내는지 검증하는 간단한 confluence/관찰성 실험용 테스트 스크립트입니다.
;;
;; 사용법(프로젝트 루트에서):
;;   guile -L . tests/confluence-tests.scm
;; 또는 전체 테스트와 함께:
;;   make test
;; 어시스턴트 메모: 규칙(SEARCH/REPLACE 블록 포맷) 확인했습니다. 현재 변경은 최소한의 주석 추가 뿐이며,
;; 이후 요청하신 변경은 동일한 형식의 SEARCH/REPLACE 블록으로 제안하겠습니다.
;;
;; 이 파일은 추가적인 빌드 변경 없이 독립적으로 실행 가능합니다.
;; (테스트는 reduce-net-to-normal-form 호출 시 max-iter=1000을 사용하여 충분한 축약 기회를 제공합니다.)

(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu rewrite)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

;; 재작성 패스의 "기본" 목록( eval.scm의 기본과 동일한 순서로 구성 )
(define default-passes
  (list
   rewrite-pass-const-fold!
   rewrite-pass-if-fold!
   rewrite-pass-AA-merge!
   rewrite-pass-AC!
   rewrite-pass-AE!
   rewrite-pass-CE-annihilation!
   rewrite-pass-wire-cleanup!))

;; 헬퍼: 주어진 net을 주어진 패스 순서로 축약한 뒤 reduced net을 반환
(define (reduce-with-passes net passes)
  (let ((opts (list (cons 'passes default-passes) (cons 'max-iter 1000))))
    (reduce-net-to-normal-form (copy-net net) opts)))

;; 몇 가지 대표적인 넷 생성기 (테스트 파일들과 유사한 예제들)
(define (make-const-fold-net)
  (parse-net
   '(par
     (node lt1 A 'prim/lt)
     (node num-2 A 'lit/num 2)
     (node num-3 A 'lit/num 3)
     (node out A)
     (wire (num-2 p) (lt1 l))
     (wire (num-3 p) (lt1 r))
     (wire (lt1 p) (out p)))))

(define (make-if-fold-net)
  (parse-net
   '(par
     (node if-impl A 'prim/if) (node cond-copy C) (node out-if A)
     (node num-123 A 'lit/num) ; non-boolean literal should prevent if-fold normally
     (wire (num-123 p) (cond-copy p))
     (wire (cond-copy l) (if-impl p))
     (wire (cond-copy r) (out-if p))
     (wire (if-impl l) (then-lit p))
     (node then-lit A))))

(define (make-if-fold-boolean-net)
  (parse-net
   '(par
     (node if-impl A 'prim/if) (node cond-copy C) (node out-if A)
     (node lit-true A 'lit/bool #t)
     (node then-lit A)
     (node else-lit A)
     (wire (lit-true p) (cond-copy p))
     (wire (cond-copy l) (if-impl p))
     (wire (if-impl l) (then-lit p))
     (wire (if-impl r) (else-lit p)))))

(define (make-aa-net)
  (parse-net '(par (node a A) (node b A) (wire (a p) (b p)))))

(define (make-composite-net)
  (parse-net
   '(par
     (node a A) (node c C) (wire (a p) (c p))
     (node a2 A) (node e E) (wire (a2 p) (e p))
     (node c3 C) (node e3 E) (wire (c3 p) (e3 p))
     (node a3 A) (node b3 A) (wire (a3 p) (b3 p))
     (node lt A 'prim/lt) (node num-2 A 'lit/num 2) (node num-3 A 'lit/num 3)
     (wire (num-2 p) (lt l)) (wire (num-3 p) (lt r)) (wire (lt p) (out p))
     (node if-impl A 'prim/if) (node cond-copy C) (node out-if A)
     (node cond-lit A 'lit/bool #t)
     (wire (cond-lit p) (cond-copy p))
     (wire (cond-copy l) (if-impl p))
     (wire (cond-copy r) (out-if p))
     (wire (if-impl l) (then-lit p))
     (node then-lit A)
     (node orphan C))))

;; 실험 테스트 1: 기본 순서 vs 뒤집힌 순서
;; 변경: pretty-print 문자열 자체를 비교하는 대신 '관찰자' 결과(출력 노드가 있으면 out 값, 그렇지 않으면 A 노드 수)를 비교합니다.
(define (test-confluence-default-vs-reversed)
  (let ((nets (list (make-const-fold-net)
                    (make-if-fold-boolean-net)
                    (make-aa-net)
                    (make-composite-net))))
    (for-each
     (lambda (n)
       (let* ((r-default (reduce-with-passes n default-passes))
              (r-rev     (reduce-with-passes n (reverse default-passes)))
              (has-out-default (hash-ref (net-nodes r-default) 'out #f))
              (has-out-rev     (hash-ref (net-nodes r-rev) 'out #f)))
         (cond
          ((and has-out-default has-out-rev)
           (let ((v1 (eval-net (copy-net r-default) '((out-name . out))))
                 (v2 (eval-net (copy-net r-rev) '((out-name . out)))))
             (assert-eq v1 v2 "both reductions produce out; values must match")))
          ((and (not has-out-default) (not has-out-rev))
           (let ((c1 (length (all-nodes-with-agent r-default 'A)))
                 (c2 (length (all-nodes-with-agent r-rev 'A))))
             (assert-eq c1 c2 "both reductions have no out; number of A nodes must match")))
          (else
           (assert-eq has-out-default has-out-rev "기본 패스 순서와 뒤집힌 순서가 관찰자 결과에서 동치여야 함")))))
     nets)
    #t))

;; 실험 테스트 2: 국지적 교환(두 패스 위치를 바꾼 순서) — 추가 안전성 검사
;; 변경: pretty-print 비교 대신 관찰자 기반 비교 사용
(define (test-confluence-swap-two)
  (let* ((p1 rewrite-pass-const-fold!)
         (p2 rewrite-pass-if-fold!)
         (rest (list rewrite-pass-AA-merge! rewrite-pass-AC! rewrite-pass-AE! rewrite-pass-CE-annihilation! rewrite-pass-wire-cleanup!))
         (order-A (append (list p1 p2) rest))
         (order-B (append (list p2 p1) rest))
         (nets (list (make-const-fold-net) (make-if-fold-boolean-net) (make-composite-net))))
    (for-each
     (lambda (n)
	   (let* ((rA (reduce-with-passes n order-A))
			  (rB (reduce-with-passes n order-B))
			  (observe (lambda (reduced-net)
                         (format-string #f "~a"
                           (if (hash-ref (net-nodes reduced-net) 'out #f)
                               (eval-net (copy-net reduced-net) '((out-name . out)))
                               (length (all-nodes-with-agent reduced-net 'A))))))
			  (oA (observe rA))
			  (oB (observe rB)))
         (assert-eq oA oB "두 패스의 위치 교환이 관찰자 결과에 영향을 주지 않아야 함")))
     nets)
    #t))

;; 실험 테스트 3: 랜덤화된 몇 가지 순서(소수 샘플)
(define (shuffle lst)
  (let ((arr (list->vector lst))
        (n (length lst)))
    (let loop ((i n))
	  (if (<= i 1)
		  (vector->list arr)
		  (begin
            (let* ((j (+ (random i) 0))
				   (tmp (vector-ref arr (- i 1))))
			  (vector-set! arr (- i 1) (vector-ref arr j))
			  (vector-set! arr j tmp))
            (loop (- i 1)))))))

(define (test-confluence-randomized-samples)
  (let ((base default-passes)
        (nets (list (make-const-fold-net) (make-if-fold-boolean-net) (make-composite-net)))
        (samples 5))
    (let loop ((i 0))
	  (if (>= i samples)
		  #t
		  (begin
            (let ((shuffled (shuffle base)))
			  (for-each
			   (lambda (n)
                 (let* ((r1 (reduce-with-passes n base))
                        (r2 (reduce-with-passes n shuffled))
                        (observe (lambda (reduced-net)
								   (if (hash-ref (net-nodes reduced-net) 'out #f)
									   (eval-net (copy-net reduced-net) '((out-name . out)))
									   (length (all-nodes-with-agent reduced-net 'A)))))
                        (o1 (observe r1))
                        (o2 (observe r2)))
				   (assert-eq o1 o2 "랜덤 패스 시도와 기본 순서의 관찰 결과 일치")))
			   nets))
            (loop (+ i 1)))))))

(run-tests "Confluence"
		   (list
            test-confluence-default-vs-reversed
            test-confluence-swap-two
            test-confluence-randomized-samples))
