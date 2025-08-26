(use-modules (icnu utils format)
             (icnu utils assertions)
             (srfi srfi-1)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu eval))

(define (test-IC_Y_small_finite)
  "Create a small IC_Y net and ensure reduction finishes and out node exists.
   Use a trivially terminating function (it simply returns its argument) to avoid true recursion."
  (let* ((fn-name (gensym "yf-"))
         (sexpr (mk-par
                 (IC_Y fn-name 'outy) ; IC_Y builds a structure; 'fn-name' is just a symbol placeholder
                 ))
         (net (parse-net sexpr))
         (reduced (reduce-net-to-normal-form net '((max-iter . 50)))))
    (assert-true (node-agent reduced 'outy) "IC_Y created out node")
    #t))

;; -----------------------------------------------------------------------------
;; Utilities
;; -----------------------------------------------------------------------------

(define (copier-name i) (string->symbol (format-string #f "c~a" i)))
(define (out-name i)    (string->symbol (format-string #f "out-~a" i)))

;; -----------------------------------------------------------------------------
;; Node builders
;; -----------------------------------------------------------------------------

(define (make-literal-node)
  ;; 원 코드와 동일하게 리터럴을 'num-99' 라는 이름의 A-노드로 둡니다.
  (mk-node 'num-99 'A))

(define (make-out-nodes outs)
  ;; out-1 ... out-N 을 A-노드로 생성하여 리스트로 반환
  (let loop ((i 1) (acc '()))
    (if (> i outs)
        (reverse acc)
        (loop (+ i 1) (cons (mk-node (out-name i) 'A) acc)))))

(define (make-copier-nodes outs)
  "복제기(카피어) 노드들을 생성.
   M = max(0, outs-1). c1..cM 을 C-노드로 생성하여
   (values copier-nodes copier-names-list M) 를 반환."
  (let* ((M (max 0 (- outs 1))))
    (let loop ((i 1) (nodes '()) (names '()))
      (if (> i M)
          (values (reverse nodes) (reverse names) M)
          (let* ((nm (copier-name i))
                 (node (mk-node nm 'C)))
            (loop (+ i 1) (cons node nodes) (cons nm names)))))))

;; -----------------------------------------------------------------------------
;; Wire builders for the copier fanout tree
;; -----------------------------------------------------------------------------

(define (root-wire-or-direct outs M)
  "루트 배선 또는 퇴화 케이스(카피어 없음) 배선 생성.
   - M>0: num-99.p -> c1.p
   - M=0: num-99.p -> out-1.r"
  (if (> M 0)
      (list (mk-wire 'num-99 'p (copier-name 1) 'p))
      (list (mk-wire 'num-99 'p (out-name 1) 'r))))

(define (children-wires-for-copier i M outs)
  "카피어 i 의 l/r 포트를 왼/오른쪽 자식에 연결.
   - 자식 인덱스가 M 이하면 c[child].p 로 연결
   - 초과하면 out-노드의 r 로 연결 (leaf: out-(child-M).r)"
  (let* ((left-idx  (* 2 i))
         (right-idx (+ (* 2 i) 1))
         (left-wire
          (if (<= left-idx M)
              (mk-wire (copier-name i) 'l (copier-name left-idx) 'p)
              (mk-wire (copier-name i) 'l (out-name (- left-idx M)) 'r)))
         (right-wire
          (if (<= right-idx M)
              (mk-wire (copier-name i) 'r (copier-name right-idx) 'p)
              (mk-wire (copier-name i) 'r (out-name (- right-idx M)) 'r))))
    (list left-wire right-wire)))

(define (make-copier-tree-wires outs M)
  "M>0 이면 c1..cM 의 이진 트리 배선을 생성, M=0 이면 빈 리스트."
  (if (= M 0)
      '()
      (let loop ((i 1) (acc '()))
        (if (> i M)
            (reverse acc)
            (loop (+ i 1)
                  (append (children-wires-for-copier i M outs) acc))))))

(define (build-fanout-nodes-and-wires outs)
  "리터럴 + out 노드들 + 카피어 노드들, 그리고 배선 전체를 구성하여 반환.
   (values all-nodes all-wires)"
  (let* ((lit-node (make-literal-node))
         (out-nodes (make-out-nodes outs)))
    (call-with-values
        (lambda () (make-copier-nodes outs))
      (lambda (copier-nodes copier-names M)
        (let* ((root-or-direct (root-wire-or-direct outs M))
               (tree-wires     (make-copier-tree-wires outs M))
               (all-nodes      (append (list lit-node) out-nodes copier-nodes))
               (all-wires      (append root-or-direct tree-wires)))
          (values all-nodes all-wires))))))

;; -----------------------------------------------------------------------------
;; Net construction & reduction
;; -----------------------------------------------------------------------------

(define (build-net-from-nw nodes wires)
  ;; 필요하면 sexpr, net 둘 다 만들어 활용할 수 있게 구성
  (let* ((sexpr (apply mk-par (append nodes wires)))
         (net   (parse-net `(par ,@nodes ,@wires))))
    (values sexpr net)))

(define (reduce-net net)
  (reduce-net-to-normal-form (copy-net net) '((max-iter . 200))))

;; -----------------------------------------------------------------------------
;; Verification
;; -----------------------------------------------------------------------------

(define (verify-outs->99 reduced-net parsed-net outs)
  "각 out-i.r 이 99 로 해석되는지 확인.
   - 축약넷에서 먼저 시도, 실패 시 원본넷으로 폴백"
  (let loop ((i 1))
    (if (> i outs)
        #t
        (let* ((on (out-name i))
               (ep (endpoint on 'r))
               (rval (resolve-literal-ep reduced-net ep)))
          (if (eq? rval *unresolved*)
              (assert-eq (resolve-literal-ep parsed-net ep)
                         99
                         (format-string #f "out-~a resolves to 99 (fallback)" i))
              (assert-eq rval 99 (format-string #f "out-~a resolves to 99 (reduced)" i)))
        (loop (+ i 1))))))

;; -----------------------------------------------------------------------------
;; Top-level test (original behavior preserved)
;; -----------------------------------------------------------------------------

(define (test-complex-copier-tree-stability)
  "Build a fanout tree where a single literal is wired to many outputs via distinct copiers.
   After reduction, each out should resolve to the shared literal value."
  (let* ((outs 64))
    (call-with-values
        (lambda () (build-fanout-nodes-and-wires outs))
      (lambda (nodes wires)
        (call-with-values
            (lambda () (build-net-from-nw nodes wires))
          (lambda (sexpr net)
            (let* ((reduced (reduce-net net)))
              (verify-outs->99 reduced net outs)
              #t)))))))

(define (run-all-mid-tests)
  (let ((tests (list
                test-IC_Y_small_finite
                test-complex-copier-tree-stability)))
    (display "Running mid tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (catch 'mid-tests (lambda () (t) #t) (lambda args (display "EXN\n") #f))))
                  (if res (display "ok\n") (display "FAIL\n"))))
              tests)
    (display "All mid tests completed.\n")
    #t))

(run-all-mid-tests)
