(use-modules (icnu utils format)
             (icnu utils strings)
             (icnu utils assertions)
             (srfi srfi-1)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu eval)
             (icnu tools icnu-inject)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)

(define (count-agent net agent)
  "Count nodes of type agent in net."
  (let ((acc 0))
    (hash-for-each (lambda (k v) (when (eq? v agent) (set! acc (+ acc 1)))) (net-nodes net))
    acc))

(define (count-nodes-with-prefix net prefix)
  "Count node symbols whose printed name begins with prefix."
  (let ((acc 0))
    (hash-for-each
     (lambda (k v)
       (when (string-prefix? prefix (symbol->string k))
         (set! acc (+ acc 1))))
     (net-nodes net))
    acc))


(define (test-IC_LITERAL-number)
  (let ((net (parse-net (IC_LITERAL 42 'out))))
    (assert-eq (eval-net net '((out-name . out))) 42 "IC_LITERAL number resolves to 42")
    #t))

(define (test-IC_LITERAL-boolean_and_triggers)
  (let ((net (parse-net (IC_LITERAL #t 'out))))
    (assert-eq (eval-net net '((out-name . out))) #t "IC_LITERAL #t resolves to #t"))
  (let ((net2 (parse-net (IC_LITERAL #f 'out2))))
    (assert-eq (eval-net net2 '((out-name . out2))) #f "IC_LITERAL #f resolves to #f"))
  (let ((net3 (parse-net (IC_LITERAL 7 'trig-lit-x))))
    (assert-true (eval-net net3 '((out-name . trig-lit-x))) "trigger-style literal resolves"))
  #t)

(define (test-IC_CONS_NIL_AND_ACCESSORS)
  (let* ((lit1 (IC_LITERAL 1 'h1))
         (lit2 (IC_LITERAL 2 'h2))
         (lit3 (IC_LITERAL 3 'h3))
         (nilf (IC_NIL 'nilx))
         (c3 (IC_CONS (list 'h3 'p) 'nilx 'cons3))
         (c2 (IC_CONS (list 'h2 'p) 'cons3 'cons2))
         (c1 (IC_CONS (list 'h1 'p) 'cons2 'cons1))
         (net (parse-net (mk-par lit1 lit2 lit3 nilf c3 c2 c1))))
    (assert-true (node-agent net 'cons1) "cons1 exists")
    (assert-true (node-agent net 'cons2) "cons2 exists")
    (assert-true (node-agent net 'cons3) "cons3 exists")
    (let ((first-net (parse-net (IC_FIRST 'cons1 'out-first))))
      (assert-true (node-agent first-net 'out-first) "IC_FIRST created out node"))
    (let ((rest-net (parse-net (IC_REST 'cons1 'out-rest))))
      (assert-true (node-agent rest-net 'out-rest) "IC_REST created out node"))
    #t))

(define (test-IC_PRIM_ADD_constant_and_symbolic)
  (let* ((sexpr `(par (node num-10 A 'lit/num 10)
                       (node num-5 A 'lit/num 5)
                       ,(IC_PRIM_ADD 'num-10 'num-5 'out)))
         (net (parse-net sexpr))
         (result (eval-net net '((out-name . out)))))
    (assert-eq result 15 "IC_PRIM_ADD with constants folds to 15"))
  (let ((net2 (parse-net (IC_PRIM_ADD (list 'x 'p) (list 'y 'p) 'out2))))
    (assert-true (node-agent net2 'out2) "symbolic add created out node")
    (assert-true (> (count-agent net2 'C) 0) "symbolic add contains copier nodes")
    #t))

(define (test-IC_APPLY_and_IC_COPY)
  (let ((net (parse-net (IC_APPLY 'f 'x 'out))))
    (assert-true (node-agent net 'out) "IC_APPLY created out node")
    (assert-true (> (count-agent net 'C) 0) "IC_APPLY contains copier nodes")
    (let ((cnet (parse-net (IC_COPY 'in 'outc))))
      (assert-true (node-agent cnet 'outc) "IC_COPY created out node")
      (assert-true (>= (count-agent cnet 'C) 2) "IC_COPY has two copier nodes"))
    #t))

(define (test-JOIN_REPLACE_AND_JOIN_MAX)
  (let ((net (parse-net (JOIN-REPLACE 'x 'y 'outj))))
    (assert-eq (peer net (endpoint 'outj 'p)) (endpoint 'y 'p) "JOIN-REPLACE wires out -> y"))
  (let ((mnet (parse-net (JOIN-MAX 'a 'b 'omax))))
    (assert-true (node-agent mnet 'omax) "JOIN-MAX created out node"))
  #t)

(define (test-IC_BOOLEAN_EVAL_direct)
  (let* ((;; --- GADGETS ---
          t1 'true1) (t2 'true2)
          (f1 'false1) (f2 'false2))
    (let* ((and-sexpr `(par ,(IC_MK_TRUE t1) ,(IC_MK_FALSE f1) ,(IC_MK_FALSE f2)
                           ,(mk-node 'out-and 'A)
                           ,(IC_IF `(,t1 p) `(,f1 p) `(,f2 p) 'out-and)))
           (or-sexpr `(par ,(IC_MK_TRUE t1) ,(IC_MK_TRUE t2) ,(IC_MK_FALSE f1)
                          ,(mk-node 'out-or 'A)
                          ,(IC_IF `(,t1 p) `(,t2 p) `(,f1 p) 'out-or)))
           (not-sexpr `(par ,(IC_MK_TRUE t1) ,(IC_MK_TRUE t2) ,(IC_MK_FALSE f1)
                           ,(mk-node 'out-not 'A)
                           ,(IC_IF `(,t1 p) `(,f1 p) `(,t2 p) 'out-not)))
           (and-net (parse-net and-sexpr))
           (or-net (parse-net or-sexpr))
           (not-net (parse-net not-sexpr)))
      (assert-eq (eval-net and-net '((out-name . out-and))) #f "Direct AND eval: #t, #f -> #f")
      (assert-eq (eval-net or-net '((out-name . out-or))) #t "Direct OR eval: #t, #f -> #t")
      (assert-eq (eval-net not-net '((out-name . out-not))) #f "Direct NOT eval: #t -> #f")))
  #t)

(define (test-IC_CHURCH_APPLY_structure)
  (let ((n 3)
		(net (parse-net (IC_CHURCH-APPLY 3 'f 'x 'outc))))
	(assert-true (node-agent net 'outc) "IC_CHURCH-APPLY created out node")
	(assert-true (>= (count-nodes-with-prefix net "church-app-") 3) "IC_CHURCH-APPLY created n app nodes")
	#t))

(define (test-IC_CHURCH-APPLY-zero)
  (let* ((net (parse-net (IC_CHURCH-APPLY 0 'f 'x 'outc)))
		 (eval-net (reduce-net-to-normal-form net)))
	(assert-eq (peer eval-net (endpoint 'outc 'p)) (endpoint 'x 'p) "IC_CHURCH-APPLY with n=0 should wire x to out"))
  #t)

(define (test-IC_Y_basic_and_endpoint)
  (let ((net (parse-net (IC_Y 'f 'outy))))
	(assert-true (node-agent net 'outy) "IC_Y created out node for symbol fn"))
  (let ((net2 (parse-net (IC_Y (list 'g 'p) 'outy2))))
	(assert-true (node-agent net2 'outy2) "IC_Y accepted endpoint-form fn"))
  #t)

(define (test-IC_EQ_LT_GT_CONST_and_copy_ops)
  (let ((eq-net (parse-net (IC_EQ_CONST 3 'in 'outeq))))
	(assert-true (node-agent eq-net 'outeq) "IC_EQ_CONST created out node")
	(let ((lt-net (parse-net (IC_LT_CONST 5 'in 'outlt))))
	  (assert-true (node-agent lt-net 'outlt) "IC_LT_CONST created out node"))
	(let ((gt-net (parse-net (IC_GT_CONST 2 'in 'outgt))))
	  (assert-true (node-agent gt-net 'outgt) "IC_GT_CONST created out node"))
	#t))

(define (test-IC_FOLD_and_placeholders)
  (let ((fnet (parse-net (IC_FOLD 'lst 'acc 'fn 'outf))))
    (assert-true (node-agent fnet 'outf) "IC_FOLD created out node")
    (let* ((sexpr `(par
                        (node num-1a A 'lit/num)
                        (node num-1b A 'lit/num)
                        ,(IC_PRIM_ADD_RUNTIME 'num-1a 'num-1b 'outp)))
           (r (parse-net sexpr)))
      (assert-true (node-agent r 'outp) "IC_PRIM_ADD_RUNTIME created out")))
  #t)

(define (test-inject-empty-list)
  (let* ((net (parse-net (generate-injection-form (list (cons 'out '()))))))
	(let ((p (peer net (endpoint 'out 'r))))
	  (assert-true (and p (eq? (node-agent net (car p)) 'A)) "inject empty list produced an A node on out.r"))
	#t))

(define (test-inject-deep-nested-list)
  (let* ((val (list 1 (list 2 (list 3 (list 4 5)))))
		 (net (parse-net (generate-injection-form (list (cons 'out val)))))
		 (cons-count (count-nodes-with-prefix net "inj-cons-")))
	(assert-true (>= cons-count 3) (format-string #f "deep nested cons chain created: ~a" cons-count))
	#t))

(define (test-church-apply-large)
  (let* ((n 20)
		 (sexpr (IC_CHURCH-APPLY n 'f 'x 'outc))
		 (net (parse-net sexpr))
		 (cnt (count-nodes-with-prefix net "church-app-")))
	(assert-true (>= cnt n) (format-string #f "church-apply created ~a app nodes (expected >= ~a)" cnt n))
	#t))


(run-tests "StdLib"
		   (list
			test-IC_LITERAL-number
			test-IC_LITERAL-boolean_and_triggers
			test-IC_CONS_NIL_AND_ACCESSORS
			test-IC_PRIM_ADD_constant_and_symbolic
			test-IC_APPLY_and_IC_COPY
			test-JOIN_REPLACE_AND_JOIN_MAX
			test-IC_BOOLEAN_EVAL_direct
			test-IC_CHURCH_APPLY_structure
			test-IC_CHURCH-APPLY-zero
			test-IC_Y_basic_and_endpoint
			test-IC_EQ_LT_GT_CONST_and_copy_ops
			test-IC_FOLD_and_placeholders
			test-inject-empty-list
			test-inject-deep-nested-list
			test-church-apply-large))
