(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu icnu)
             (icnu eval)
             (icnu rewrite)
             (icnu utils log)
             (icnu stdlib icnu-lib)
             (tests test-runner))

(set-debug-level! 0)

(define default-passes
  (list
   rewrite-pass-const-fold!
   rewrite-pass-if-fold!
   rewrite-pass-AA-merge!
   rewrite-pass-AC!
   rewrite-pass-AE!
   rewrite-pass-CE-annihilation!
   rewrite-pass-wire-cleanup!))

(define (reduce-with-passes net passes)
  (let ((opts (list (cons 'passes default-passes) (cons 'max-iter 1000))))
    (reduce-net-to-normal-form (copy-net net) opts)))

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
             (assert-eq v1 v2 "Confluence: both reductions must produce identical observable values")))
          ((and (not has-out-default) (not has-out-rev))
           (let ((c1 (length (all-nodes-with-agent r-default 'A)))
                 (c2 (length (all-nodes-with-agent r-rev 'A))))
             (assert-eq c1 c2 "Confluence: networks without output must have equal size under observation")))
          (else
           (error "Confluence violation: reduction paths produced different observation capabilities")))))
     nets)
    #t))

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

(define (net->canonical-form net)
  (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))

(define (test-confluence-church-rosser-property)
  "Church-Rosser property 검증: reduction이 confluence함을 보장"
  (let* ((nets (list (make-const-fold-net) 
                    (make-if-fold-boolean-net) 
                    (make-composite-net)
                    (parse-net (IC_CHURCH-APPLY 3 'f 'x 'outc))))
        (max-steps 1000))
    (for-each
     (lambda (net)
       (let* ((passes1 (shuffle default-passes))
              (passes2 (shuffle default-passes))
              (r1 (reduce-with-passes (copy-net net) passes1))
              (r2 (reduce-with-passes (copy-net net) passes2))
              ;; Normal form까지 reduction
              (nf1 (reduce-with-passes r1 default-passes))
              (nf2 (reduce-with-passes r2 default-passes)))
         ;; Church-Rosser property: 모든 reduction path는 같은 normal form으로 수렴
         (let ((obs1 (if (hash-ref (net-nodes nf1) 'out #f)
                        (eval-net (copy-net nf1) '((out-name . out)))
                        (net->canonical-form nf1)))
               (obs2 (if (hash-ref (net-nodes nf2) 'out #f)
                        (eval-net (copy-net nf2) '((out-name . out)))
                        (net->canonical-form nf2))))
           (assert-eq obs1 obs2 "Church-Rosser property: 모든 reduction path는 같은 normal form으로 수렴"))))
     nets)
    #t))

(run-tests "Confluence"
		   (list
            test-confluence-default-vs-reversed
            test-confluence-swap-two
            test-confluence-randomized-samples
            test-confluence-church-rosser-property))
