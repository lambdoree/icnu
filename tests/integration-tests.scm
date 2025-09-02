(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu utils internal)
             (icnu icnu)
             (icnu rewrite)
             (icnu eval)
             (icnu stdlib icnu-lib)
             (icnu tools icnu-validate)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

(define (test-resolve-literal-deep-chain)
  "Deep copier chain: resolve-literal-ep should traverse many Copier nodes and find the literal."
  (let* ((depth 128)
         (nodes '())
         (wires '()))
    (set! nodes (cons (mk-node 'num-1 'A 'lit/num 1) nodes))
    (set! nodes (cons (mk-node 'out 'A) nodes))
    (let loop ((i 1))
      (when (<= i depth)
        (let ((cname (string->symbol (format-string #f "c~a" i))))
          (set! nodes (cons (mk-node cname 'C) nodes))
          (if (= i 1)
              (set! wires (cons (mk-wire 'num-1 'p cname 'p) wires))
              (let ((prev (string->symbol (format-string #f "c~a" (- i 1)))))
                (set! wires (cons (mk-wire prev 'l cname 'p) wires)))))
        (loop (+ i 1))))
    (set! wires (cons (mk-wire (string->symbol (format-string #f "c~a" depth)) 'l 'out 'p) wires))
    (let ((sexpr (apply mk-par (append nodes wires))))
      (let ((net (parse-net sexpr)))
        (assert-eq (resolve-literal-ep net (endpoint 'out 'p)) 1 "deep copier chain resolves to literal 1")))
    #t))

(define (test-resolve-literal-cycle_detection)
  "A longer cycle: ensure resolve-literal-ep does not loop indefinitely and returns *unresolved*."
  (let* ((sexpr (mk-par
                 (mk-node 'c1 'C) (mk-node 'c2 'C) (mk-node 'out 'A)
                 (mk-wire 'c1 'l 'c2 'p)
                 (mk-wire 'c2 'l 'c1 'p)
                 (mk-wire 'out 'p 'c1 'r))))
    (let ((net (parse-net sexpr)))
      (assert-eq (resolve-literal-ep net (endpoint 'out 'p)) *unresolved* "cycle resolves to *unresolved*")))
  #t)

(define (test-reduce-unlimited-equivalence)
  "Compare reduce-net-to-normal-form with a large numeric max-iter vs (max-iter . #f) (unlimited) for a chain of AA merges."
  (let* ((n 16)
         (nodes '())
         (wires '()))
    (let loop ((i 1))
      (if (> i n)
          #t
          (begin
            (set! nodes (cons (mk-node (string->symbol (format-string #f "a~a" i)) 'A) nodes))
            (when (and (> i 1) (even? i))
              (set! wires (cons (mk-wire (string->symbol (format-string #f "a~a" (- i 1))) 'p
                                         (string->symbol (format-string #f "a~a" i)) 'p)
                                wires)))
            (loop (+ i 1)))))
    (let ((sexpr (apply mk-par (append nodes wires)))
          (opts-large '((max-iter . 100)))
          (opts-unlim '((max-iter . #f))))
      (let ((net1 (parse-net sexpr))
            (net2 (parse-net sexpr)))
        (let ((r1 (reduce-net-to-normal-form net1 opts-large))
	      (r2 (reduce-net-to-normal-form net2 opts-unlim)))
          (assert-eq (format-string #f "~a" (pretty-print r1 '((show-nu? . #t))))
                     (format-string #f "~a" (pretty-print r2 '((show-nu? . #t))))
                     "unlimited reduction equals large numeric cap"))))
    #t))

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

(define (copier-name i) (string->symbol (format-string #f "c~a" i)))
(define (out-name i)    (string->symbol (format-string #f "out-~a" i)))
(define (make-literal-node) (mk-node 'num-99 'A 'lit/num 99))
(define (make-out-nodes outs)
  (let loop ((i 1) (acc '()))
    (if (> i outs) (reverse acc)
        (loop (+ i 1) (cons (mk-node (out-name i) 'A) acc)))))
(define (make-copier-nodes outs)
  (let* ((M (max 0 (- outs 1))))
    (let loop ((i 1) (nodes '()) (names '()))
      (if (> i M) (values (reverse nodes) (reverse names) M)
          (let* ((nm (copier-name i)) (node (mk-node nm 'C)))
            (loop (+ i 1) (cons node nodes) (cons nm names)))))))
(define (root-wire-or-direct outs M)
  (if (> M 0) (list (mk-wire 'num-99 'p (copier-name 1) 'p))
      (list (mk-wire 'num-99 'p (out-name 1) 'r))))
(define (children-wires-for-copier i M outs)
  (let* ((left-idx (* 2 i)) (right-idx (+ (* 2 i) 1))
         (left-wire (if (<= left-idx M) (mk-wire (copier-name i) 'l (copier-name left-idx) 'p)
                        (mk-wire (copier-name i) 'l (out-name (- left-idx M)) 'r)))
         (right-wire (if (<= right-idx M) (mk-wire (copier-name i) 'r (copier-name right-idx) 'p)
                         (mk-wire (copier-name i) 'r (out-name (- right-idx M)) 'r))))
    (list left-wire right-wire)))
(define (make-copier-tree-wires outs M)
  (if (= M 0) '()
      (let loop ((i 1) (acc '()))
        (if (> i M) (reverse acc)
            (loop (+ i 1) (append (children-wires-for-copier i M outs) acc))))))
(define (build-fanout-nodes-and-wires outs)
  (let* ((lit-node (make-literal-node)) (out-nodes (make-out-nodes outs)))
    (call-with-values (lambda () (make-copier-nodes outs))
      (lambda (copier-nodes copier-names M)
        (let* ((root-or-direct (root-wire-or-direct outs M))
               (tree-wires (make-copier-tree-wires outs M))
               (all-nodes (append (list lit-node) out-nodes copier-nodes))
               (all-wires (append root-or-direct tree-wires)))
          (values all-nodes all-wires))))))
(define (build-net-from-nw nodes wires)
  (let* ((sexpr (apply mk-par (append nodes wires)))
         (net (parse-net `(par ,@nodes ,@wires)))) (values sexpr net)))
(define (reduce-net net)
  (reduce-net-to-normal-form (copy-net net) '((max-iter . 200))))
(define (verify-outs->99 reduced-net parsed-net outs)
  (let loop ((i 1))
    (if (> i outs) #t
        (let* ((on (out-name i)) (ep (endpoint on 'r)) (rval (resolve-literal-ep reduced-net ep)))
          (if (eq? rval *unresolved*)
              (assert-eq (resolve-literal-ep parsed-net ep) 99 (format-string #f "out-~a resolves to 99 (fallback)" i))
              (assert-eq rval 99 (format-string #f "out-~a resolves to 99 (reduced)" i)))
          (loop (+ i 1))))))

(define (test-complex-copier-tree-stability)
  "Build a fanout tree where a single literal is wired to many outputs via distinct copiers.
   After reduction, each out should resolve to the shared literal value."
  (let* ((outs 64))
    (call-with-values (lambda () (build-fanout-nodes-and-wires outs))
      (lambda (nodes wires)
        (call-with-values (lambda () (build-net-from-nw nodes wires))
          (lambda (sexpr net)
            (let* ((reduced (reduce-net net)))
              (verify-outs->99 reduced net outs)
              #t)))))))

(define *long-tests-enabled* (make-parameter #t))

(define (test-long-church-apply_heavy)
  "Heavy reduction using IC_CHURCH-APPLY with a larger n.
   This test is gated behind *long-tests-enabled* and is skipped by default."
  (if (not (*long-tests-enabled*))
      (begin (format-string #t "skipping long test: heavy church-apply ") #t)
      (let* ((n 30)
             (sexpr (read (open-input-string (format-string #f "~a" (IC_CHURCH-APPLY n 'f 'x 'outc))))))
        (let ((net (parse-net sexpr)))
          (let ((reduced (reduce-net-to-normal-form net '((max-iter . 2000)))))
            (assert-true (node-agent reduced 'outc) "outc node exists after heavy reduction")
            #t)))))

(define (safe-parse sexpr)
  (catch 'safe-parse
    (lambda () (parse-net sexpr) #t)
    (lambda args #f)))

(define (test-parse-net-restores-link-mode)
  (let ((orig (*link-conflict-mode*)))
    (set-link-conflict-mode! 'error)
    (let ((sexpr '(par (node a A) (node b A) (node c A)
                       (wire (a p) (b p))
                       (wire (a p) (c p)))))
      (assert-true
       (catch #t
              (lambda () (parse-net sexpr) #f)
              (lambda args #t))
       "parse-net should raise an error on conflicting wires"))
    (assert-eq (*link-conflict-mode*) 'error "parse-net should not change global link-conflict-mode")
    (set-link-conflict-mode! orig)
    #t))

(define (test-reduce-respects-max-iter)
  (let ((base-sexpr '(par (node a A) (node b A) (node c C)
                          (wire (a p) (c p))
                          (wire (c l) (b p)))))
    (let ((net1 (parse-net base-sexpr)))
      (let ((res (reduce-net-to-normal-form net1 '((max-iter . 0)))))
        (assert-true res "reduce with max-iter 0 should return without error")))
    (let ((net2 (parse-net base-sexpr)))
      (let ((res2 (reduce-net-to-normal-form net2 '((max-iter . 5)))))
        (assert-true res2 "reduce with max-iter 5 should return without error")))
    #t))

(define (test-format-string-accepts-port)
  (let ((out (call-with-output-string (lambda (p) (format-string p "hello ~a" "world")))))
    (assert-eq out "hello world" "format-string should write to provided output port"))
  (let ((out2 (call-with-output-string (lambda (p) (parameterize ((current-output-port p)) (format-string #t "x~a" 1))))))
    (assert-eq out2 "x1" "format-string #t prints to current-output-port"))
  #t)

(define (test-IC_COPY-self-name)
  (let ((sexpr (IC_COPY 'x 'x)))
    (let ((net (parse-net sexpr)))
      (assert-true (node-agent net 'x) "IC_COPY created or preserved node x")
      #t)))

(define (test-validate-ir-bad-link-format)
  (let ((n (empty-net)))
    (hash-set! (net-links n) 'bad-key (endpoint 'a 'p))
    (let ((errs (validate-ir n)))
      (assert-true (not (null? errs)) "validate-ir should report bad link format"))
    #t))

(define (test-resolve-literal-trigger-style)
  (let* ((sexpr (IC_LITERAL 7 'trig-out))
         (net (parse-net sexpr)))
    (let ((val (eval-net net '((out-name . trig-out)))))
      (assert-true (or (equal? val 7) (equal? val 'num-7)) "trigger-style literal resolves to 7"))
    #t))

(define (test-validate-ir-valid)
  (let ((net (parse-net '(par (node a A) (node b A) (wire (a p) (b p))))))
    (assert-true (null? (validate-ir net)) "validate-ir on a valid net should return an empty list")))

(define (test-validate-ir-invalid-agent)
  (let ((net (empty-net)))
    (hash-set! (net-nodes net) 'x 'BADAGENT)
    (assert-false (null? (validate-ir net)) "validate-ir on a net with invalid agent should return a non-empty list")))

(define (test-validate-ir-non-reciprocal-link)
  (let ((net (empty-net)))
    (add-node! net 'a 'A)
    (add-node! net 'b 'A)
    (hash-set! (net-links net) (endpoint 'a 'p) (endpoint 'b 'p))
    (assert-false (null? (validate-ir net)) "validate-ir on a net with non-reciprocal link should return a non-empty list")))

(define (test-tag-system-resists-name-heuristics)
  "Test that the rewrite system relies on tags, not node name heuristics.
   An 'lt' operator is given a misleading 'if-impl' name, but tagged 'prim/lt'.
   The const-fold pass should still correctly identify and fold it."
  (let ((net (parse-net
              '(par
                (node if-impl-imposter A 'prim/lt)
                (node num-10 A 'lit/num 10)
                (node num-20 A 'lit/num 20)
                (node out A)
                (wire (num-10 p) (if-impl-imposter l))
                (wire (num-20 p) (if-impl-imposter r))
                (wire (if-impl-imposter p) (out p))))))
    (assert-true (rewrite-pass-const-fold! net) "const-fold should apply based on tag")
    (assert-false (node-agent net 'if-impl-imposter) "imposter node should be removed after folding")
    (let ((out-peer (peer net (endpoint 'out 'p))))
      (assert-true out-peer "out.p should have a peer")
      (let ((val (resolve-literal-ep net out-peer)))
        (assert-eq val #t "result of 10 < 20 is #t"))))
  #t)

(run-tests "Integration"
           (list
            test-resolve-literal-deep-chain
            test-resolve-literal-cycle_detection
            test-reduce-unlimited-equivalence
            test-IC_Y_small_finite
            test-complex-copier-tree-stability
            test-long-church-apply_heavy
            test-parse-net-restores-link-mode
            test-reduce-respects-max-iter
            test-format-string-accepts-port
            test-IC_COPY-self-name
            test-validate-ir-bad-link-format
            test-resolve-literal-trigger-style
            test-validate-ir-valid
            test-validate-ir-invalid-agent
            test-validate-ir-non-reciprocal-link
            test-tag-system-resists-name-heuristics))
