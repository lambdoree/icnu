(use-modules (icnu utils format)
             (icnu utils strings)
             (icnu utils assertions)
             (icnu utils internal)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu eval)
             (icnu tools icnu-inject)
             (icnu utils log)
             (icnu utils compat)
             (tests test-runner))

(set-debug-level! 0)

(define (flatten-ic-forms forms)
  (apply append
    (map (lambda (form)
           (if (and (list? form) (not (memq (car form) '(quote wire node par nu))))
               (flatten-ic-forms form)
               (list form)))
         forms)))

(define (wire-or-list src dst . maybe-port)
  (let ((port (if (null? maybe-port) 'p (car maybe-port))))
    (if (symbol? src)
        `(wire (,src p) (,dst ,port))
        `(wire ,src (,dst ,port)))))

(define (local-IC_GENERIC_CONST_OP const-val in-port out-node tag)
  (let ((in-copy (icnu-gensym "in-copy-")))
    `((node ,out-node A ,tag)
      (node ,in-copy C)
      ,(wire-or-list in-port in-copy)
      (wire (,in-copy l) (,out-node l)))))

(define (local-ICNU_GENERIC_CONST_OP const-val in-port out-node tag)
  (let ((lit (icnu-gensym "lit-")))
    `(par
      ,(ICNU_LITERAL const-val lit)
      ,@(local-IC_GENERIC_CONST_OP const-val in-port out-node tag)
      (wire (,lit p) (,out-node r)))))

(define (local-ICNU_EQ_CONST const-val in-port out-node)
  (local-ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/eq))

(define (local-ICNU_LT_CONST const-val in-port out-node)
  (local-ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/lt))

(define (local-ICNU_GT_CONST const-val in-port out-node)
  (local-ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/gt))

(define (unwrap-value v)
  (cond
   ((and (pair? v) (eq? (car v) 'value)) (cdr v))
   ((and (list? v) (assq 'value v)) (cdr (assq 'value v)))
   (else v)))

(define (count-agent net agent)
  (let ((acc 0))
    (hash-for-each (lambda (k v) (when (eq? v agent) (set! acc (+ acc 1)))) (net-nodes net))
    acc))

(define (count-nodes-with-prefix net prefix)
  (let ((acc 0))
    (hash-for-each
     (lambda (k v)
       (when (string-prefix? prefix (symbol->string k))
         (set! acc (+ acc 1))))
     (net-nodes net))
    acc))


(define (test-ICNU_LITERAL-number)
  (let ((net (parse-net (ICNU_LITERAL 42 'out))))
    (assert-eq (eval-net net '((out-name . out))) 42 "ICNU_LITERAL number resolves to 42")
    #t))

(define (test-ICNU_LITERAL-boolean_and_triggers)
  (let ((net (parse-net (ICNU_LITERAL #t 'out))))
    (assert-eq (eval-net net '((out-name . out))) #t "ICNU_LITERAL #t resolves to #t"))
  (let ((net2 (parse-net (ICNU_LITERAL #f 'out2))))
    (assert-eq (unwrap-value (eval-net net2 '((out-name . out2)))) #f "ICNU_LITERAL #f resolves to #f"))
  (let ((net3 (parse-net (ICNU_LITERAL 7 'trig-lit-x))))
    (assert-true (eval-net net3 '((out-name . trig-lit-x))) "trigger-style literal resolves"))
  #t)

(define (test-ICNU_CONS_NIL_AND_ACCESSORS)
  (let* ((lit1 (ICNU_LITERAL 1 'h1))
         (lit2 (ICNU_LITERAL 2 'h2))
         (lit3 (ICNU_LITERAL 3 'h3))
         (nilf (ICNU_NIL 'nilx))
         (c3 (ICNU_CONS (list 'h3 'p) 'nilx 'cons3))
         (c2 (ICNU_CONS (list 'h2 'p) 'cons3 'cons2))
         (c1 (ICNU_CONS (list 'h1 'p) 'cons2 'cons1))
         (net (parse-net `(par ,lit1 ,lit2 ,lit3 ,nilf ,c3 ,c2 ,c1))))
    (assert-true (node-agent net 'cons1) "cons1 exists")
    (assert-true (node-agent net 'cons2) "cons2 exists")
    (assert-true (node-agent net 'cons3) "cons3 exists")
    (let ((first-net (parse-net (ICNU_FIRST 'cons1 'out-first))))
      (assert-true (node-agent first-net 'out-first) "ICNU_FIRST created out node"))
    (let ((rest-net (parse-net (ICNU_REST 'cons1 'out-rest))))
      (assert-true (node-agent rest-net 'out-rest) "ICNU_REST created out node"))
    #t))

(define (test-ICNU_PRIM_ADD_constant_and_symbolic)
  (let* ((sexpr `(par (node num-10 A 'lit/num 10)
                      (node num-5 A 'lit/num 5)
                      ,(ICNU_PRIM_ADD 'num-10 'num-5 'out)))
         (net (parse-net sexpr))
         (result (unwrap-value (eval-net net '((out-name . out))))))
    (assert-eq result 15 "ICNU_PRIM_ADD with constants folds to 15"))
  (let ((net2 (parse-net (ICNU_PRIM_ADD (list 'x 'p) (list 'y 'p) 'out2))))
    (assert-true (node-agent net2 'out2) "symbolic add created out node")
    (assert-true (> (count-agent net2 'C) 0) "symbolic add contains copier nodes")
    #t))

(define (test-ICNU_APPLY_and_ICNU_COPY)
  (let ((net (parse-net (ICNU_APPLY 'f 'x 'out))))
    (assert-true (node-agent net 'out) "ICNU_APPLY created out node")
    (assert-true (> (count-agent net 'C) 0) "ICNU_APPLY contains copier nodes")
    (let ((cnet (parse-net (ICNU_COPY 'in 'outc))))
      (assert-true (node-agent cnet 'outc) "ICNU_COPY created out node")
      (assert-true (>= (count-agent cnet 'C) 2) "ICNU_COPY has two copier nodes"))
    #t))

(define (test-JOIN_REPLACE_AND_JOIN_MAX)
  (let ((net (parse-net (ICNU_JOIN-REPLACE 'x 'y 'outj))))
    (assert-eq (peer net (endpoint 'outj 'p)) (endpoint 'y 'p) "JOIN-REPLACE wires out -> y"))
  (let ((mnet (parse-net (ICNU_JOIN-MAX 'a 'b 'omax))))
    (assert-true (node-agent mnet 'omax) "JOIN-MAX created out node"))
  #t)

(define (test-ICNU_BOOLEAN_EVAL_direct)
  (let* ((;; --- GADGETS ---
          t1 'true1) (t2 'true2)
          (f1 'false1) (f2 'false2))
    (let* ((and-sexpr `(par ,(ICNU_MK_TRUE t1) ,(ICNU_MK_FALSE f1) ,(ICNU_MK_FALSE f2)
                            (node out-and A)
                            ,(ICNU_IF `(,t1 p) `(,f1 p) `(,f2 p) 'out-and)))
           (or-sexpr `(par ,(ICNU_MK_TRUE t1) ,(ICNU_MK_TRUE t2) ,(ICNU_MK_FALSE f1)
                           (node out-or A)
                           ,(ICNU_IF `(,t1 p) `(,t2 p) `(,f1 p) 'out-or)))
           (not-sexpr `(par ,(ICNU_MK_TRUE t1) ,(ICNU_MK_TRUE t2) ,(ICNU_MK_FALSE f1)
                            (node out-not A)
                            ,(ICNU_IF `(,t1 p) `(,f1 p) `(,t2 p) 'out-not)))
           (and-net (parse-net and-sexpr))
           (or-net (parse-net or-sexpr))
           (not-net (parse-net not-sexpr)))
      (assert-eq (unwrap-value (eval-net and-net '((out-name . out-and)))) #f "Direct AND eval: #t, #f -> #f")
      (assert-eq (unwrap-value (eval-net or-net '((out-name . out-or)))) #t "Direct OR eval: #t, #f -> #t")
      (assert-eq (unwrap-value (eval-net not-net '((out-name . out-not)))) #f "Direct NOT eval: #t -> #f")))
  #t)

(define (test-ICNU_CHURCH_APPLY_structure)
  (let ((n 3)
		    (net (parse-net (flatten-ic-forms (ICNU_CHURCH-APPLY 3 'f 'x 'outc)))))
	  (assert-true (node-agent net 'outc) "ICNU_CHURCH-APPLY created out node")
	  (assert-true (>= (count-nodes-with-prefix net "church-app-") 3) "ICNU_CHURCH-APPLY created n app nodes")
	  #t))

(define (test-ICNU_CHURCH-APPLY-zero)
  (let* ((net (parse-net (ICNU_CHURCH-APPLY 0 'f 'x 'outc)))
		     (eval-net (reduce-net-to-normal-form net)))
	  (assert-eq (peer eval-net (endpoint 'outc 'p)) (endpoint 'x 'p) "ICNU_CHURCH-APPLY with n=0 should wire x to out"))
  #t)

(define (test-ICNU_Y_basic_and_endpoint)
  (let* ((y-form (ICNU_Y 'f 'outy))
         (sexpr (cons 'par (cddr y-form)))
         (net (parse-net sexpr)))
	  (assert-true (node-agent net 'outy) "ICNU_Y created out node for symbol fn"))
  (let* ((y-form2 (ICNU_Y (list 'g 'p) 'outy2))
         (sexpr2 (cons 'par (cddr y-form2)))
         (net2 (parse-net sexpr2)))
	  (assert-true (node-agent net2 'outy2) "ICNU_Y accepted endpoint-form fn"))
  #t)

(define (test-ICNU_EQ_LT_GT_CONST_and_copy_ops)
  (let ((eq-net (parse-net (local-ICNU_EQ_CONST 3 'in 'outeq))))
	  (assert-true (node-agent eq-net 'outeq) "ICNU_EQ_CONST created out node")
	  (let ((lt-net (parse-net (local-ICNU_LT_CONST 5 'in 'outlt))))
	    (assert-true (node-agent lt-net 'outlt) "ICNU_LT_CONST created out node"))
	  (let ((gt-net (parse-net (local-ICNU_GT_CONST 2 'in 'outgt))))
	    (assert-true (node-agent gt-net 'outgt) "ICNU_GT_CONST created out node"))
	  #t))

(define (test-ICNU_FOLD_and_placeholders)
  (let* ((fold-form (ICNU_FOLD 'lst 'acc 'fn 'outf))
         (sexpr (cons 'par (cddr fold-form)))
         (fnet (parse-net sexpr)))
    (assert-true (node-agent fnet 'outf) "ICNU_FOLD created out node")
    (let* ((sexpr `(par
                    (node num-1a A 'lit/num)
                    (node num-1b A 'lit/num)
                    ,(ICNU_PRIM_ADD_RUNTIME 'num-1a 'num-1b 'outp)))
           (r (parse-net sexpr)))
      (assert-true (node-agent r 'outp) "ICNU_PRIM_ADD_RUNTIME created out")))
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
		     (sexpr (ICNU_CHURCH-APPLY n 'f 'x 'outc))
		     (net (parse-net (flatten-ic-forms sexpr)))
		     (cnt (count-nodes-with-prefix net "church-app-")))
	  (assert-true (>= cnt n) (format-string #f "church-apply created ~a app nodes (expected >= ~a)" cnt n))
	  #t))
(define (test-church-numeral-properties)
  (let* ((net0 (parse-net (ICNU_CHURCH-APPLY 0 'f 'x 'out0)))
         (reduced0 (reduce-net-to-normal-form net0 '((max-iter . 100)))))
    (let ((peer-ep (peer reduced0 (endpoint 'out0 'p))))
      (assert-eq (car peer-ep) 'x "Church 0 should be identity")))
  (let* ((net1 (parse-net (flatten-ic-forms (ICNU_CHURCH-APPLY 1 'f 'x 'out1))))
         (reduced1 (reduce-net-to-normal-form net1 '((max-iter . 100)))))
    (assert-true (> (count-nodes-with-prefix reduced1 "church-app-") 0)
                 "Church 1 should create application structure"))
  (let* ((n 3)
         (net (parse-net (flatten-ic-forms (ICNU_CHURCH-APPLY n 'f 'x 'outc))))
         (cnt (count-nodes-with-prefix net "church-app-")))
    (assert-true (>= cnt n)
                 (format-string #f "Church numeral ~a should have at least ~a app nodes" n n)))
  #t)


(run-tests "StdLib"
		       (list
			      test-ICNU_LITERAL-number
			      test-ICNU_LITERAL-boolean_and_triggers
			      test-ICNU_CONS_NIL_AND_ACCESSORS
			      test-ICNU_PRIM_ADD_constant_and_symbolic
			      test-ICNU_APPLY_and_ICNU_COPY
			      test-JOIN_REPLACE_AND_JOIN_MAX
			      test-ICNU_BOOLEAN_EVAL_direct
			      test-ICNU_CHURCH_APPLY_structure
			      test-ICNU_CHURCH-APPLY-zero
			      test-ICNU_Y_basic_and_endpoint
			      test-ICNU_EQ_LT_GT_CONST_and_copy_ops
			      test-ICNU_FOLD_and_placeholders
			      test-inject-empty-list
			      test-inject-deep-nested-list
			      test-church-apply-large
			      test-church-numeral-properties))
