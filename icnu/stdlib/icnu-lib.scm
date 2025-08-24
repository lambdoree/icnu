(define-module (icnu stdlib icnu-lib)
  #:use-module (icnu icnu)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils helpers)
  #:export (IC_TRUE IC_FALSE IC_IF IC_Y
		    IC_CHURCH-ENCODE
		    JOIN-REPLACE JOIN-MAX
		    ;; Net-building helpers
		    IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		    IC_AND IC_OR IC_NOT IC_COPY
		    IC_PRIM_ADD IC_PRIM_ADD_RUNTIME IC_APPLY
		    ;; Boolean Gadget constructors
		    IC_MK_TRUE IC_MK_FALSE
            ;; from church-runtime
            IC_CHURCH-RUN IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD))

;; --- Booleans ---
;; TRUE selects the first branch (left on an Applicator).
(define IC_TRUE
  (mk-nu '(e b)
         (mk-par
          (mk-node 'b 'A)
          (mk-node 'e 'E)
          (mk-wire 'b 'r 'e 'p))))

;; FALSE selects the second branch (right on an Applicator).
(define IC_FALSE
  (mk-nu '(e b)
         (mk-par
          (mk-node 'b 'A)
          (mk-node 'e 'E)
          (mk-wire 'b 'l 'e 'p))))

;; IF c t e -> out
(define (IC_IF c-port t-port e-port out-node)
  (let* ((if-impl (gensym "if-impl-"))
         (c-c (gensym "cond-copy-"))
         (t-c (gensym "then-copy-"))
         (e-c (gensym "else-copy-"))
         (out-c (gensym "out-copy-")))
    `(nu (,if-impl ,c-c ,t-c ,e-c ,out-c)
         (par
          ;; The final output node is a clean Applicator.
          ,(mk-node out-node 'A)
          
          ;; Internal applicator that performs the IF selection.
          ,(mk-node if-impl 'A)
          
          ;; Copy the condition. Default to 'p' port for symbols (cells), but respect port if given as a pair.
          ,(mk-node c-c 'C)
          ,(wire-or-list c-port c-c)
          ,(mk-wire c-c 'l if-impl 'p)

          ;; Copy then/else branches from their 'p' port (booleans are on 'p').
          ,(mk-node t-c 'C)
          ,(wire-or-list t-port t-c)
          ,(mk-wire t-c 'l if-impl 'l)

          ,(mk-node e-c 'C)
          ,(wire-or-list e-port e-c)
          ,(mk-wire e-c 'l if-impl 'r)
          
          ;; Use a copier to expose the result on the 'r' port of the out-node.
          ,(mk-node out-c 'C)
          ,(mk-wire if-impl 'p out-c 'p)
          ,(mk-wire out-c 'l out-node 'r)
          ))))


;; --- Church Numerals ---
(define (IC_CHURCH-ENCODE n)
  (let ((sym (string->symbol (format-string #f "church-~a" n))))
    (mk-node sym 'A)))

;; --- Lattice Joins ---
;; JOIN-REPLACE(x, y) = y
(define (JOIN-REPLACE x y out)
  (let ((e (gensym "erase_")))
    (mk-nu (list e)
           (mk-par (mk-node e 'E)
                   (mk-wire e 'p x 'p)
                   (mk-wire out 'p y 'p)))))

;; JOIN-MAX(x, y) = if x > y then x else y
(define (JOIN-MAX x y out)
  ;; Placeholder for a comparator net
  (let ((comp (gensym "max_comp_")))
    (mk-nu (list comp)
           (mk-par (mk-node comp 'A)
                   (mk-wire comp 'p out 'p)))))

;; --- Net-building helpers for propagators ---
(define (IC_LITERAL val out)
  (cond
   ((boolean? val)
    (let ((lit-node (gensym "lit-bool-")))
      `(nu (,lit-node)
           (par
            ,(if val (IC_MK_TRUE lit-node) (IC_MK_FALSE lit-node))
            ,(mk-node out 'A)
            ,(mk-wire lit-node 'p out 'r)))))
   ((number? val)
    (let* ((is-trigger (string-prefix? "trig-lit-" (symbol->string out)))
           (lit (if is-trigger
                    (gensym (format-string #f "trig-num-~a-" val))
                    (string->symbol (format-string #f "num-~a" val)))))
      (if (eq? lit out)
          `(par ,(mk-node out 'A))
          (let ((body `(par ,(mk-node lit 'A) ,(mk-node out 'A) ,(mk-wire lit 'p out 'r))))
            (if is-trigger `(nu (,lit) ,body) body)))))
   ((or (symbol? val) (string? val))
    (let* ((is-trigger (string-prefix? "trig-lit-" (symbol->string out)))
           (str-val (if (symbol? val) (symbol->string val) val))
           (safe-str (string-join-list (string-split-char str-val #\space) "_"))
           (lit (if is-trigger
                    (gensym (string-append "trig-str-" safe-str "-"))
                    (string->symbol (string-append "str-" safe-str)))))
      (if (eq? lit out)
          `(par ,(mk-node out 'A))
          (let ((body `(par ,(mk-node lit 'A) ,(mk-node out 'A) ,(mk-wire lit 'p out 'r))))
            (if is-trigger `(nu (,lit) ,body) body)))))
   (else
    (error "IC_LITERAL: unsupported literal type" val))))

(define (IC_APPLY f-port x-port out)
  (let* ((out-node (if (symbol? out) out out))
         (c-f (gensym "app-f-"))
         (c-x (gensym "app-x-")))
    `(nu (,c-f ,c-x)
         (par
          ,(mk-node out-node 'A)
          ,(mk-node c-f 'C)
          ,(mk-node c-x 'C)
          ,(wire-or-list f-port c-f)
          ,(wire-or-list x-port c-x)
          ,(mk-wire c-f 'l out-node 'l)
          ,(mk-wire c-x 'l out-node 'r)))))

(define (IC_GENERIC_CONST_OP const-val in-port out-node)
  (let* ((lit (gensym "lit-"))
         (in-copy (gensym "in-copy-"))
         (norm-in-port (if (symbol? in-port) (list in-port 'p) in-port)))
    `(nu (,lit ,in-copy)
         (par ,(IC_LITERAL const-val lit)
              ,(mk-node out-node 'A)
              ,(mk-node in-copy 'C)
              ,(list 'wire norm-in-port (list in-copy 'p))
              ,(mk-wire in-copy 'l out-node 'l)
              ,(mk-wire lit 'p out-node 'r)))))

(define (IC_EQ_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

(define (IC_LT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

(define (IC_GT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node))

(define (IC_COPY in out)
  (let ((c-left (gensym "c-left-"))
        (c-right (gensym "c-right-")))
    `(nu (,c-left ,c-right)
	 (par
          ;; The output gadget 'out' is a fresh Applicator.
          ,(mk-node out 'A)
          ;; Copy the left branch of 'in' to the left branch of 'out'.
          ,(mk-node c-left 'C)
          ,(mk-wire in 'l c-left 'p)
          ,(mk-wire c-left 'l out 'l)
          ;; Copy the right branch of 'in' to the right branch of 'out'.
          ,(mk-node c-right 'C)
          ,(mk-wire in 'r c-right 'p)
          ,(mk-wire c-right 'l out 'r)))))

(define (IC_AND in1-port in2-port out-node)
  ;; out = IF in1 THEN in2 ELSE FALSE
  (let ((false-node (gensym "false-"))
        (in2-copy (gensym "in2-copy-")))
    `(nu (,false-node ,in2-copy)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #f false-node)
          ;; copy in2 from its principal port to match IC_IF's `then` expectation
          ,(mk-node in2-copy 'C)
          ,(wire-or-list in2-port in2-copy)
          ,(IC_IF in1-port (list in2-copy 'l) (list false-node 'r) out-node)))))

(define (IC_OR in1-port in2-port out-node)
  ;; out = IF in1 THEN TRUE ELSE in2
  (let ((true-node (gensym "true-"))
        (in2-copy (gensym "in2-copy-")))
    `(nu (,true-node ,in2-copy)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #t true-node)
          ;; copy in2 from its principal port to match IC_IF's `else` expectation
          ,(mk-node in2-copy 'C)
          ,(wire-or-list in2-port in2-copy)
          ,(IC_IF in1-port (list true-node 'r) (list in2-copy 'l) out-node)))))

(define (IC_NOT in-port out-node)
  ;; out = IF in THEN FALSE ELSE TRUE
  (let ((true-node (gensym "true-"))
        (false-node (gensym "false-")))
    `(nu (,true-node ,false-node)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #t true-node)
          ,(IC_LITERAL #f false-node)
          ,(IC_IF in-port (list false-node 'r) (list true-node 'r) out-node)))))

(define (IC_PRIM_ADD in1 in2 out)
  (let* ((s1 (if (symbol? in1) (symbol->string in1) (symbol->string (car in1))))
         (s2 (if (symbol? in2) (symbol->string in2) (symbol->string (car in2))))
         )
    (cond
     ;; church + church -> church-(n+m)
     ((and (string-prefix? "church-" s1)
           (string-prefix? "church-" s2))
      (let* ((n1 (string->number (substring s1 7)))
             (n2 (string->number (substring s2 7)))
             (sum (+ (if n1 n1 0) (if n2 n2 0)))
             (res (string->symbol (format-string #f "church-~a" sum))))
        `(nu (,res)
             (par
              ,(mk-node res 'A)
              ,(mk-node out 'A)
              ;; expose the computed literal on out's aux port
              ,(mk-wire res 'p out 'r)))))
     ;; num + num -> num-(n+m)
     ((and (string-prefix? "num-" s1)
           (string-prefix? "num-" s2))
      (let* ((n1 (string->number (substring s1 4)))
             (n2 (string->number (substring s2 4)))
             (sum (+ (if n1 n1 0) (if n2 n2 0)))
             (res (string->symbol (format-string #f "num-~a" sum))))
        `(nu (,res)
             (par
              ,(mk-node res 'A)
              ,(mk-node out 'A)
              ,(mk-wire res 'p out 'r)))))
     (else
      (let ((add-impl (gensym "add-"))
            (c1 (gensym "c-")) (c2 (gensym "c-"))
            (out-c (gensym "out-copy-")))
        `(nu (,add-impl ,c1 ,c2 ,out-c)
             (par
              ,(mk-node add-impl 'A)
              ,(mk-node c1 'C)
              ,(mk-node c2 'C)
              ,(if (symbol? in1) (mk-wire in1 'p c1 'p) (list 'wire in1 (list c1 'p)))
              ,(if (symbol? in2) (mk-wire in2 'p c2 'p) (list 'wire in2 (list c2 'p)))
              ,(mk-wire c1 'l add-impl 'l)
              ,(mk-wire c2 'l add-impl 'r)
              ,(mk-node out 'A)
              ,(mk-node out-c 'C)
              ,(mk-wire add-impl 'p out-c 'p)
              ,(mk-wire out-c 'l out 'r))))))))

(define (IC_MK_TRUE b)
  (let ((e (gensym "e_")))
    `(nu (,e)
         (par
          ,(mk-node b 'A)
          ,(mk-node e 'E)
          ,(mk-wire b 'r e 'p)))))

(define (IC_MK_FALSE b)
  (let ((e (gensym "e_")))
    `(nu (,e)
         (par
          ,(mk-node b 'A)
          ,(mk-node e 'E)
          ,(mk-wire b 'l e 'p)))))

(define (IC_Y fn out)
  (let* ((y-node   (gensym "Y"))
         (dup-node (gensym "Ydup"))
         (app1     (gensym "Yapp1"))
         (app2     (gensym "Yapp2"))
         (res-node (gensym "Yres")))
    ;; Build explicit surface s-expression (lists) to avoid quasiquote issues.
    (list 'nu (list y-node dup-node app1 app2 res-node)
          (list 'par
                ;; nodes
                (mk-node y-node 'A)
                (mk-node dup-node 'C)
                (mk-node app1 'A)
                (mk-node app2 'A)
                (mk-node res-node 'A)
                (wire-or-list fn dup-node)

                ;; duplicator outputs: supply two copies to app1 and app2
                (mk-wire dup-node 'l app1 'p)
                (mk-wire dup-node 'r app2 'p)

                ;; app1: receives argument via y-node's left port
                (mk-wire app1 'l y-node 'l)
                ;; app1's result is the argument for app2
                (mk-wire app1 'r app2 'l)

                ;; app2's result is wired back to y-node's right port to form the recursion loop
                (mk-wire app2 'r y-node 'r)

                ;; app1 produces a principal result; collect it at res-node
                (mk-wire app1 'p res-node 'p)

                ;; expose final result at out.p
                (mk-wire res-node 'p out 'p)))))


(define (IC_CHURCH-RUN n out)
  (let ((sym (string->symbol (format-string #f "church-~a" n))))
    `(par
      ,(mk-node sym 'A)
      ,(mk-node out 'A)
      ,(mk-wire sym 'p out 'p))))

(define (IC_CHURCH-APPLY church-id f-port x-port out-target)
  (let* ((n (cond ((number? church-id) church-id)
                  ((and (symbol? church-id)
                        (let ((s (symbol->string church-id)))
                          (and (>= (string-length s) 7) (string=? (substring s 0 7) "church-"))))
                   (string->number (substring (symbol->string church-id) 7)))
                  (else #f))))
    (cond
     ((eq? n #f)
      (error "IC_CHURCH-APPLY: unsupported church identifier" church-id))
     ((<= n 0)
      ;; Zero: identity -> wire x to out
      (let ((from-ep (if (symbol? x-port) (list x-port 'p) x-port))
            (to-ep (if (symbol? out-target) (list out-target 'p) out-target)))
        `(par ,@(if (symbol? out-target) (list (mk-node out-target 'A)) '())
              (wire ,from-ep ,to-ep))))
     (else
      ;; Build n applicator nodes and a copier-tree that fans `f` to the left ports.
      (letrec ((build-fanout
                (lambda (input-port k)
                  (if (<= k 1)
                      (cons (list input-port) #f)
                      (let* ((c-node (gensym "copier-"))
                             (num-left (ceiling (/ k 2)))
                             (num-right (floor (/ k 2)))
                             (left-result (build-fanout (list c-node 'l) num-left))
                             (right-result (build-fanout (list c-node 'r) num-right))
                             (outputs (append (car left-result) (car right-result)))
                             (left-net (cdr left-result))
                             (right-net (cdr right-result))
                             (children (filter (lambda (x) x) (list left-net right-net))))
                        (cons outputs
                              `(nu (,c-node)
                                 (par
                                  ,(mk-node c-node 'C)
                                  ,(if (symbol? input-port)
                                       (mk-wire input-port 'p c-node 'p)
                                       (list 'wire input-port (list c-node 'p)))
                                  ,@children))))))))
        (let* ((apps (letrec ((loop (lambda (k acc)
                                      (if (= k 0)
                                          (reverse acc)
                                          (loop (- k 1) (cons (gensym "church-app-") acc))))))
                       (loop n '())))
               (app-node-forms (map (lambda (nm) (mk-node nm 'A)) apps))
               (fan (build-fanout f-port n))
               (outputs (car fan))
               (copier-net (cdr fan))
               (left-wires
                (map (lambda (pair app)
                       (let ((src-name (car pair))
                             (src-port (cadr pair)))
                         (mk-wire src-name src-port app 'l)))
                     outputs apps))
               (chain-wires
                (letrec ((loop (lambda (lst acc)
                                 (if (null? (cdr lst))
                                     (reverse acc)
                                     (let ((a (car lst)) (b (cadr lst)))
                                       (loop (cdr lst) (cons (mk-wire a 'r b 'p) acc)))))))
                  (loop apps '())))
               (last (car (reverse apps)))
               (last-wire (if (symbol? x-port)
                              (mk-wire last 'r x-port 'p)
                              (list 'wire (list last 'r) x-port)))
               (out-wire (if (symbol? out-target)
                             (mk-wire (car apps) 'p out-target 'p)
                             (list 'wire (list (car apps) 'p) out-target))))
          ;; assemble final s-expression
          `(nu ,apps
               (par
                ,@(if copier-net (list copier-net) '())
                ,@app-node-forms
                ,@left-wires
                ,@chain-wires
                ,last-wire
                ,@(if (symbol? out-target) (list (mk-node out-target 'A)) '())
                ,out-wire))))))))

(define (IC_PRIM_ADD_RUNTIME in1 in2 out)
  (IC_PRIM_ADD in1 in2 out))

(define (IC_CONS head tail name)
  (let ((sym name))
    `(par
      ,(mk-node sym 'A)
      ,(if (symbol? head)
           (mk-wire head 'p sym 'l)
           (list 'wire head (list sym 'l)))
      ,(if (symbol? tail)
           (mk-wire tail 'p sym 'r)
           (list 'wire tail (list sym 'r))))))

(define (IC_NIL name)
  (let ((sym (if (symbol? name) name (string->symbol (format-string #f "nil-~a" name))))
        (e1 (gensym "e_"))
        (e2 (gensym "e_")))
    (list 'nu (list sym e1 e2)
          (list 'par
                (mk-node sym 'A)
                (mk-node e1 'E)
                (mk-node e2 'E)
                (mk-wire sym 'l e1 'p)
                (mk-wire sym 'r e2 'p)))))

(define (IC_FIRST cons out)
  `(par ,(mk-node out 'A) ,(mk-wire cons 'l out 'p)))

(define (IC_REST cons out)
  `(par ,(mk-node out 'A) ,(mk-wire cons 'r out 'p)))

(define (IC_FOLD list-port acc-port fn-port out)
  (let ((fold-n (gensym "fold-")))
    `(nu (,fold-n)
	 (par
          ,(mk-node fold-n 'A)
          ,(if (symbol? list-port) (mk-wire list-port 'p fold-n 'l) (list 'wire list-port 'p fold-n 'l))
          ,(if (symbol? acc-port) (mk-wire acc-port 'p fold-n 'r) (list 'wire acc-port 'p fold-n 'r))
          ,(mk-node out 'A)))))
