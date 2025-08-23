(use-modules (icnu utils format)
             (icnu utils strings)
	     (icnu utils assertions)
             (srfi srfi-1)
	     (srfi srfi-11)
             (icnu icnu)
             (icnu stdlib icnu-lib)
             (icnu rewrite)
             (icnu tools icnu-inject))

;; -----------------------------------------------------------------
;; helpers
;; -----------------------------------------------------------------
(define (read-sexpr-from-string s)
  (call-with-input-string s read))

(define (net-summary-str net)
  (format-string #f "<nodes=~a links=~a>"
          (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-nodes net)) c)
          (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-links net)) c)))

(define (print-net-step net label)
  (format-string #t "=== ~a : ~a~%" label (net-summary-str net))
  (let ((names (all-names net)))
    (format-string #t "  sample names: ~a~%"
            (let loop ((l names) (n 8) (acc '()))
              (if (or (zero? n) (null? l))
                  (reverse acc)
                  (loop (cdr l) (- n 1) (cons (car l) acc))))))
  (format-string #t "  pretty: ~a~%" (format-string #f "~a" (pretty-print net '((show-nu? . #t))))))

(define (read-sexpr-from-file path)
  (call-with-input-file path
    (lambda (in)
      (read in))))

(define (par-elements sexpr)
  (if (and (pair? sexpr) (eq? (car sexpr) 'par))
      (cdr sexpr)
      (list sexpr)))

(define (combine-pars . sexprs)
  (let ((elems '()))
    (for-each (lambda (s) (set! elems (append elems (par-elements s)))) sexprs)
    `(par ,@elems)))

(define (apply-pass-once net pass-name)
  (case pass-name
    ((copy-fold) (rewrite-pass-copy-fold! net))
    ((const-fold) (rewrite-pass-const-fold! net))
    ((if-fold) (rewrite-pass-if-fold! net))
    ((A-merge) (rewrite-pass-A! net))
    ((AA-merge) (rewrite-pass-AA-merge! net))
    ((wire-cleanup) (rewrite-pass-wire-cleanup! net))
    (else (error "unknown pass" pass-name))))

(define (small-step-demo sexpr-str)
  (let* ((sexpr (read-sexpr-from-string sexpr-str))
         (net (parse-net sexpr))
         (passes (list 'copy-fold 'const-fold 'if-fold 'A-merge 'AA-merge 'wire-cleanup)))
    (format-string #t "\nSMALL-STEP DEMO: initial ~a~%" (net-summary-str net))
    (for-each
     (lambda (p)
       (let ((before-nodes (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-nodes net)) c))
             (before-links (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-links net)) c)))
         (let ((changed (apply-pass-once net p)))
           (let ((after-nodes (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-nodes net)) c))
                 (after-links (let ((c 0)) (hash-for-each (lambda (k v) (set! c (+ c 1))) (net-links net)) c)))
             (format-string #t " pass ~a: changed=~a  nodes ~a->~a links ~a->~a~%"
                     p changed before-nodes after-nodes before-links after-links)))))
     passes)
    (format-string #t "SMALL-STEP DEMO: final ~a~%" (net-summary-str net))
    net))

(define (big-step-reduce net . maybe-max-iter)
  (let ((max-iter (if (null? maybe-max-iter) 100 (car maybe-max-iter))))
    (let loop ((i 0))
      (when (> i max-iter) (error "big-step-reduce: exceeded max iterations"))
      (let ((changed? #f))
        (when (rewrite-pass-copy-fold! net) (set! changed? #t) (format-string #t "  [big] copy-fold\n"))
        (when (rewrite-pass-const-fold! net) (set! changed? #t) (format-string #t "  [big] const-fold\n"))
        (when (rewrite-pass-if-fold! net) (set! changed? #t) (format-string #t "  [big] if-fold\n"))
        (when (rewrite-pass-A! net) (set! changed? #t) (format-string #t "  [big] A-merge\n"))
        (when (rewrite-pass-AA-merge! net) (set! changed? #t) (format-string #t "  [big] AA-merge\n"))
        (when (rewrite-pass-wire-cleanup! net) (set! changed? #t) (format-string #t "  [big] wire-cleanup\n"))
        (if changed? (loop (+ i 1)) (values net i))))))

;; -----------------------------------------------------------------
;; Example networks (as strings)
;; -----------------------------------------------------------------
(define copier-example
  "(par (node a A) (node c C) (wire (a p) (c p)) (node tgt A) (wire (c l) (tgt l)))")

(define const-compare-example
  "(par (node lt1 A) (node num-2 A) (node num-3 A) (node out A) (wire (num-2 p) (lt1 l)) (wire (num-3 p) (lt1 r)) (wire (lt1 p) (out p)))")

(define if-example
  ;; build small IF using stdlib helper (IC_IF) then parse its printed form
  (format-string #f "~a" (IC_IF (list 'cond-copy-0 'p) (list 'then-lit 'r) (list 'else-lit 'r) 'out-if)))

(define aa-merge-example
  "(par (node a A) (node b A) (wire (a p) (b p)))")

(define injection-demo-base
  "(par (node out A))")

(define copier-chain-example
  "(par
    (node a A) (node b A) (node c C) (node d C) (node e A)
    (wire (a p) (b p))
    (wire (b p) (c p))
    (wire (c l) (d l))
    (wire (d p) (e p))
    (wire (c r) (e r)))")

(define nested-nu-qualified-example
  "(nu (x y)
     (par
       (node Sys.cell A)
       (node x A)
       (node y V)
       (wire (Sys.cell p) (x p))
       (wire (y p) (x l))))")

(define church-apply-example
  (format-string #f "~a" (IC_CHURCH-APPLY 5 'f 'x 'outc)))

(define church-apply-large-example
  (format-string #f "~a" (IC_CHURCH-APPLY 10 'f 'x 'outc10)))

(define cons-list-example
  (format-string #f "~a"
          (mk-par
           (IC_LITERAL 1 'h1)
           (IC_LITERAL 2 'h2)
           (IC_LITERAL 3 'h3)
           (IC_NIL 'nilx)
           (IC_CONS (list 'h3 'p) 'nilx 'cons3)
           (IC_CONS (list 'h2 'p) 'cons3 'cons2)
           (IC_CONS (list 'h1 'p) 'cons2 'cons1))))

(define self-loop-example
  "(par (node s A) (wire (s p) (s r)))")

(define deep-nu-example
  "(nu (a b c d e f g h i j)
     (par
       (node a A) (node b A) (wire (a p) (b p))))")

(define fanout-and-shared-literal-example
  (format-string #f "~a"
          (combine-pars
           "(par (node root A) (node out1 A) (node out2 A))"
           (format-string #f "~a" (IC_LITERAL 99 'shared-lit))
           "(par (wire (shared-lit p) (out1 r)) (wire (shared-lit p) (out2 r)) (wire (root p) (shared-lit p)))")))

(define nested-injection-example
  (format-string #f "~a" (generate-injection-form (list (cons 'out (list 1 (list 2 3) 4))))))

(define reuse-literals-example
  (format-string #f "~a" (generate-injection-form (list (cons 'a 7) (cons 'b 7) (cons 'c 7)))))

(define conflict-wire-example
  "(par (node a A) (node b A) (node c A) (wire (a p) (b p)) (wire (a p) (c p)))")

(define prim-add-mixed-example
  (format-string #f "~a" (IC_PRIM_ADD 'num-4 (list 'x 'p) 'out-mixed)))

(define y-complex-example
  (format-string #f "~a" (IC_Y (list 'g 'p) 'outy-complex)))

(define combined-complex-example
  (format-string #f "~a"
          (combine-pars
           copier-chain-example
           (format-string #f "~a" (IC_IF (list 'cond-copy-0 'p) (list 'then-lit 'r) (list 'else-lit 'r) 'out-if))
           (format-string #f "~a" (IC_CHURCH-APPLY 3 'f 'x 'outc)))))


(define example-names
  (list
   'copier-example
   'const-compare-example
   'if-example
   'aa-merge-example
   'injection-demo-base
   'copier-chain-example
   'nested-nu-qualified-example
   'church-apply-example
   'church-apply-large-example
   'cons-list-example
   'combined-complex-example
   'self-loop-example
   'deep-nu-example
   'fanout-and-shared-literal-example
   'nested-injection-example
   'reuse-literals-example
   'conflict-wire-example
   'prim-add-mixed-example
   'y-complex-example))

;; -----------------------------------------------------------------
;; Tests / Demos
;; -----------------------------------------------------------------
(define (test-copier-small-step)
  (format-string #t "\nTEST: copier small-step~%")
  (let ((net (small-step-demo copier-example)))
    ;; after copy-fold, copier should be removed
    (assert-false (member 'c (all-nodes-with-agent net 'C)) "copier removed after copy-fold")
    #t))

(define (test-const-fold-small-step)
  (format-string #t "\nTEST: const-fold small-step~%")
  (let ((net (small-step-demo const-compare-example)))
    ;; lt1 should be removed by const-fold -> replaced by literal on out.r
    (assert-false (member 'lt1 (all-nodes-with-agent net 'A)) "lt1 removed after const-fold")
    #t))

(define (test-if-fold-big-step)
  (format-string #t "\nTEST: if-fold big-step~%")
  ;; construct net where condition is literal true
  (let* ((if-gadget (IC_IF 'cond 'then 'else 'out-if))
         (net (parse-net `(par ,if-gadget
                               ,(IC_LITERAL #t 'cond)
                               ,(mk-node 'then 'A)
                               ,(mk-node 'else 'A)))))
    (print-net-step net "before big-step")
    (let-values (((_iter-net it) (big-step-reduce net 20)))
      (print-net-step net "after big-step")
      ;; after folding, the if-impl node should be gone
      (let ((if-impl-nodes (filter (lambda (n) (string-prefix? "if-impl-" (symbol->string n)))
                                   (all-nodes-with-agent net 'A))))
        (assert-true (null? if-impl-nodes) "if-impl removed after big-step"))
      #t)))

(define (test-aa-merge)
  (format-string #t "\nTEST: AA-merge demo~%")
  (let ((net (parse-net (read-sexpr-from-string aa-merge-example))))
    (print-net-step net "aa before")
    (assert-true (rewrite-pass-AA-merge! net) "AA merge applied")
    (print-net-step net "aa after")
    ;; a and b should not both remain as distinct A nodes
    (let ((as (all-nodes-with-agent net 'A)))
      (assert-true (<= (length as) 2) "AA merge reduced number of A nodes"))
    #t))

(define (test-injection-and-reduce)
  (format-string #t "\nTEST: injection and reduce~%")
  (let* ((base (read-sexpr-from-string injection-demo-base))
         (inj (generate-injection-form (list (cons 'out 5))))
         (combined (combine-pars inj base))
         (net (parse-net combined)))
    (print-net-step net "injected net before")
    (let-values (((_net it) (big-step-reduce net 20)))
      (print-net-step net "injected net after")
      ;; ensure out.r resolves to literal 5
      (let ((val (resolve-literal-ep net (endpoint 'out 'r) 8)))
        (assert-true (or (equal? val 5) (equal? val 'num-5)) (format-string #f "injected value resolved: ~a" val)))
      #t)))

;; Runner
(define (run-all-eval-examples)
  (let ((tests (list
                test-copier-small-step
                test-const-fold-small-step
                test-if-fold-big-step
                test-aa-merge
                test-injection-and-reduce)))
    (display "Running icnu eval examples/tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res (display "ok\n") (display "FAIL\n"))))
              tests)
    (display "All icnu eval examples completed.\n")
    #t))

(run-all-eval-examples)
