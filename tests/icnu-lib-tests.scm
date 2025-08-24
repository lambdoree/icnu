(use-modules (icnu utils format)
             (icnu utils strings)
	     (icnu utils assertions)
             (srfi srfi-1)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu tools icnu-inject))

;; -- Helpers ----------------------------------------------------------
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

;; -- Individual tests -----------------------------------------------

(define (test-IC_LITERAL-number)
  (let ((net (parse-net (IC_LITERAL 42 'out))))
    (assert-eq (peer net (endpoint 'out 'r)) (endpoint 'num-42 'p) "IC_LITERAL number wires num-42 -> out.r")
    (assert-eq (node-agent net 'num-42) 'A "num-42 node created as A")
    #t))

(define (test-IC_LITERAL-boolean_and_triggers)
  (let ((net (parse-net (IC_LITERAL #t 'out))))
    (assert-eq (resolve-literal-ep net (endpoint 'out 'r)) #t "IC_LITERAL #t resolves to #t"))
  (let ((net2 (parse-net (IC_LITERAL #f 'out2))))
    (assert-eq (resolve-literal-ep net2 (endpoint 'out2 'r)) #f "IC_LITERAL #f resolves to #f"))
  (let ((net3 (parse-net (IC_LITERAL 7 'trig-lit-x))))
    (assert-true (resolve-literal-ep net3 (endpoint 'trig-lit-x 'r)) "trigger-style literal resolves"))
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
  (let ((net (parse-net (IC_PRIM_ADD 'num-10 'num-5 'out))))
    (assert-eq (peer net (endpoint 'out 'r)) (endpoint 'num-15 'p) "IC_PRIM_ADD folded 10+5 -> num-15"))
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

(define (test-IC_AND_OR_NOT_structure)
  (let ((and-net (parse-net (IC_AND 'in1 'in2 'outand))))
    (assert-true (node-agent and-net 'outand) "IC_AND created out node"))
  (let ((or-net (parse-net (IC_OR 'in1 'in2 'outor))))
    (assert-true (node-agent or-net 'outor) "IC_OR created out node"))
  (let ((not-net (parse-net (IC_NOT 'in 'outnot))))
    (assert-true (node-agent not-net 'outnot) "IC_NOT created out node"))
  #t)

(define (test-IC_CHURCH_helpers_basic)
  (let ((enc (parse-net (IC_CHURCH-ENCODE 2))))
    (assert-true (node-agent enc 'church-2) "IC_CHURCH-ENCODE creates node"))
  (let ((r (parse-net (IC_CHURCH-RUN 3 'outc))))
    (assert-true (node-agent r 'outc) "IC_CHURCH-RUN created out node"))
  #t)

(define (test-IC_CHURCH_APPLY_structure)
  ;; build church-apply structure for n=3 and ensure it produces out node and at least 3 app nodes
  (let ((n 3)
        (net (parse-net (IC_CHURCH-APPLY 3 'f 'x 'outc))))
    (assert-true (node-agent net 'outc) "IC_CHURCH-APPLY created out node")
    (assert-true (>= (count-nodes-with-prefix net "church-app-") 3) "IC_CHURCH-APPLY created n app nodes")
    #t))

(define (test-IC_Y_basic_and_endpoint)
  ;; basic: symbol fn
  (let ((net (parse-net (IC_Y 'f 'outy))))
    (assert-true (node-agent net 'outy) "IC_Y created out node for symbol fn"))
  ;; fn provided as endpoint pair
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
    ;; ensure IC_PRIM_ADD_RUNTIME delegates without error
    (let ((r (parse-net (IC_PRIM_ADD_RUNTIME 'num-1 'num-1 'outp))))
      (assert-true (node-agent r 'outp) "IC_PRIM_ADD_RUNTIME created out"))
    #t))

;; Additional tests: edge cases and stress cases
(define (test-inject-empty-list)
  (format-string #t "\nTEST: inject-empty-list~%")
  (let* ((net (parse-net (generate-injection-form (list (cons 'out '()))))))
    ;; A peer connected to out.r must exist, and that node must be an Applicator (A).
    (let ((p (peer net (endpoint 'out 'r))))
      (assert-true (and p (eq? (node-agent net (car p)) 'A)) "inject empty list produced an A node on out.r"))
    #t))

(define (test-inject-deep-nested-list)
  (format-string #t "\nTEST: inject-deep-nested-list~%")
  (let* ((val (list 1 (list 2 (list 3 (list 4 5)))))
         (net (parse-net (generate-injection-form (list (cons 'out val)))))
         (cons-count (count-nodes-with-prefix net "inj-cons-")))
    ;; A deeply nested list should create multiple inj-cons- nodes.
    (assert-true (>= cons-count 3) (format-string #f "deep nested cons chain created: ~a" cons-count))
    #t))

(define (test-church-apply-large)
  (format-string #t "\nTEST: church-apply-large~%")
  (let* ((n 20)
         (sexpr (IC_CHURCH-APPLY n 'f 'x 'outc))
         (net (parse-net sexpr))
         (cnt (count-nodes-with-prefix net "church-app-")))
    (assert-true (>= cnt n) (format-string #f "church-apply created ~a app nodes (expected >= ~a)" cnt n))
    #t))

;; -- Runner ---------------------------------------------------------
(define (run-all-icnu-lib-tests)
  (let ((tests (list
                test-IC_LITERAL-number
                test-IC_LITERAL-boolean_and_triggers
                test-IC_CONS_NIL_AND_ACCESSORS
                test-IC_PRIM_ADD_constant_and_symbolic
                test-IC_APPLY_and_IC_COPY
                test-JOIN_REPLACE_AND_JOIN_MAX
                test-IC_AND_OR_NOT_structure
                test-IC_CHURCH_helpers_basic
                test-IC_CHURCH_APPLY_structure
                test-IC_Y_basic_and_endpoint
                test-IC_EQ_LT_GT_CONST_and_copy_ops
                test-IC_FOLD_and_placeholders
                test-inject-empty-list
                test-inject-deep-nested-list
                test-church-apply-large
                )))
    (display "Running expanded icnu-stdlib unit tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res
		      (display "ok\n")
		      (display "FAIL\n"))))
	      tests)
    (display "All icnu-stdlib tests completed.\n")
    #t))

(run-all-icnu-lib-tests)
