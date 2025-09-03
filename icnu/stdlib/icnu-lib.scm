(define-module (icnu stdlib icnu-lib)
  #:use-module (icnu icnu)
  #:use-module (icnu utils compat)
  ;; compat: delegates gensym/gensyms to icnu-gensym/icnu-gensyms when needed
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils helpers)
  #:export (IC_TRUE IC_FALSE IC_IF IC_Y
		    JOIN-REPLACE JOIN-MAX
		    IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		    IC_AND IC_OR IC_NOT IC_COPY
		    IC_PRIM_ADD IC_PRIM_ADD_RUNTIME IC_APPLY
		    IC_MK_TRUE IC_MK_FALSE
            IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD
			IC_PURE_ID IC_PURE_PAIR IC_PURE_FST IC_PURE_SND IC_PURE_LEFT IC_PURE_RIGHT
			IC_PURE_EITHER
                        normalize-ep ensure-number gensyms))

;; Provide a local gensym wrapper that delegates to compat's icnu-gensym,
;; ensuring portability when running on Scheme implementations without native gensym.
(define (gensym . maybe-prefix)
  (apply icnu-gensym maybe-prefix))

(define (mk-node name agent . args)
  `(node ,name ,agent ,@args))

(define (mk-wire a p b q) `(wire (,a ,p) (,b ,q)))

(define (mk-par . elts) `(par ,@elts))

(define (mk-nu names body) `(nu ,names ,body))

(define IC_TRUE
  (mk-nu '(b)
         (mk-par
          (mk-node 'b 'A 'lit/bool #t))))

(define IC_FALSE
  (mk-nu '(b)
         (mk-par
          (mk-node 'b 'A 'lit/bool #f))))

(define (IC_IF c-port t-port e-port out-node)
  (let* ((if-impl (gensym "if-impl-"))
         (c-c (gensym "cond-copy-"))
         (t-c (gensym "then-copy-"))
         (e-c (gensym "else-copy-")))
    `(nu (,if-impl ,c-c ,t-c ,e-c)
         (par
          ,(mk-node out-node 'A)
          
          ,(mk-node if-impl 'A 'prim/if)
          
          ,(mk-node c-c 'C)
          ,(wire-or-list c-port c-c)
          ,(mk-wire c-c 'l if-impl 'p)

          ,(mk-node t-c 'C)
          ,(wire-or-list t-port t-c)
          ,(mk-wire t-c 'l if-impl 'l)

          ,(mk-node e-c 'C)
          ,(wire-or-list e-port e-c)
          ,(mk-wire e-c 'l if-impl 'r)
          
          ,(mk-wire c-c 'r out-node 'p)
          ))))


(define (JOIN-REPLACE x y out)
  (let ((e (gensym "erase_")))
    (mk-nu (list e)
           (mk-par (mk-node e 'E)
                   (mk-wire e 'p x 'p)
                   (mk-wire out 'p y 'p)))))

(define (JOIN-MAX x y out)
  (let ((comp (gensym "max_comp_")))
    (mk-nu (list comp)
           (mk-par (mk-node comp 'A)
                   (mk-wire comp 'p out 'p)))))

(define (IC_LITERAL val out)
  (cond
   ((boolean? val)
    `(par ,(mk-node out 'A 'lit/bool val)))
   ((number? val)
    `(par ,(mk-node out 'A 'lit/num val)))
   ((or (symbol? val) (string? val))
    `(par ,(mk-node out 'A 'lit/str (if (symbol? val) (list 'quote val) val))))
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

(define (IC_GENERIC_CONST_OP const-val in-port out-node tag)
  (let* ((lit (gensym "lit-"))
         (in-copy (gensym "in-copy-")))
    `(nu (,lit ,in-copy)
         (par ,(IC_LITERAL const-val lit)
              ,(mk-node out-node 'A tag)
              ,(mk-node in-copy 'C)
              ,(wire-or-list in-port in-copy)
              ,(mk-wire in-copy 'l out-node 'l)
              ,(mk-wire lit 'p out-node 'r)))))

(define (IC_EQ_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/eq))

(define (IC_LT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/lt))

(define (IC_GT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/gt))

(define (IC_COPY in out)
  (if (eq? in out)
      `(par ,(mk-node out 'A))
      (let ((c-left (gensym "c-left-"))
            (c-right (gensym "c-right-")))
        `(nu (,c-left ,c-right)
             (par
              ,(mk-node out 'A)
              ,(mk-node c-left 'C)
              ,(mk-wire in 'l c-left 'p)
              ,(mk-wire c-left 'l out 'l)
              ,(mk-node c-right 'C)
              ,(mk-wire in 'r c-right 'p)
              ,(mk-wire c-right 'l out 'r))))))

(define (IC_AND in1-port in2-port out-node)
  `(par
    ,(mk-node out-node 'A)
    ,(IC_IF in1-port in2-port in1-port out-node)))

(define (IC_OR in1-port in2-port out-node)
  `(par
    ,(mk-node out-node 'A)
    ,(IC_IF in1-port in1-port in2-port out-node)))

(define (IC_NOT in-port out-node)
  (let ((true-node (gensym "true-"))
        (false-node (gensym "false-")))
    `(nu (,true-node ,false-node)
         (par
          ,(mk-node out-node 'A)
          ,(IC_LITERAL #t true-node)
          ,(IC_LITERAL #f false-node)
          ,(IC_IF in-port (list false-node 'r) (list true-node 'r) out-node)))))

(define (IC_PRIM_ADD in1 in2 out)
  (let ((add-impl (gensym "add-"))
        (c1 (gensym "c-")) (c2 (gensym "c-"))
        (out-c (gensym "out-copy-")))
    `(nu (,add-impl ,c1 ,c2 ,out-c)
         (par
          ,(mk-node add-impl 'A 'prim/add)
          ,(mk-node c1 'C)
          ,(mk-node c2 'C)
          ,(wire-or-list in1 c1)
          ,(wire-or-list in2 c2)
          ,(mk-wire c1 'l add-impl 'l)
          ,(mk-wire c2 'l add-impl 'r)
          ,(mk-node out 'A)
          ,(mk-node out-c 'C)
          ,(mk-wire add-impl 'p out-c 'p)
          ,(mk-wire out-c 'l out 'r)))))

(define (IC_MK_TRUE b)
  `(par ,(mk-node b 'A 'lit/bool #t)))

(define (IC_MK_FALSE b)
  `(par ,(mk-node b 'A 'lit/bool #f)))

(define (IC_Y fn out)
  (let* ((y-node   (gensym "Y"))
         (dup-node (gensym "Ydup"))
         (app1     (gensym "Yapp1"))
         (app2     (gensym "Yapp2"))
         (res-node (gensym "Yres")))
    (mk-nu (list y-node dup-node app1 app2 res-node)
           (mk-par
            (mk-node y-node 'A)
            (mk-node dup-node 'C)
            (mk-node app1 'A)
            (mk-node app2 'A)
            (mk-node res-node 'A)
            (wire-or-list fn dup-node)

            (mk-wire dup-node 'l app1 'p)
            (mk-wire dup-node 'r app2 'p)

            (mk-wire app1 'l y-node 'l)
            (mk-wire app1 'r app2 'l)

            (mk-wire app2 'r y-node 'r)

            (mk-wire y-node 'p res-node 'l)

            (mk-wire res-node 'p out 'p)))))


(define (normalize-ep maybe-ep default-port)
  (if (symbol? maybe-ep) (list maybe-ep default-port) maybe-ep))

(define (ensure-number n who)
  (unless (number? n)
    (error (string-append who ": first argument must be a number") n)))

(define (gensyms prefix n)
  (let loop ((k n) (acc '()))
    (if (= k 0) (reverse acc)
        (loop (- k 1) (cons (gensym prefix) acc)))))

(define (church-zero-net x-port out-target)
  (let ((from-ep (normalize-ep x-port 'p))
        (to-ep   (normalize-ep out-target 'p)))
    `(par ,@(if (symbol? out-target) (list (mk-node out-target 'A)) '())
          (wire ,from-ep ,to-ep))))

(define (build-copier-fanout input-ep k)
  (letrec ((go (lambda (in k)
                 (if (<= k 1)
                     (cons (list in) #f)
                     (let* ((c (gensym "copier-"))
                            (nl (ceiling (/ k 2)))
                            (nr (floor   (/ k 2)))
                            (L (go (list c 'l) nl))
                            (R (go (list c 'r) nr))
                            (outs (append (car L) (car R)))
                            (netL (cdr L)) (netR (cdr R))
                            (kids (filter (lambda (x) x) (list netL netR))))
                       (cons outs
                             `(nu (,c)
                                (par
                                 ,(mk-node c 'C)
                                 ,(wire-or-list in c)
                                 ,@kids))))))))
    (go input-ep k)))

(define (make-apps n)
  (gensyms "church-app-" n))

(define (app-node-forms apps)
  (map (lambda (nm) (mk-node nm 'A)) apps))

(define (left-wires outputs apps)
  (map (lambda (pair app)
         (let ((src-name (car pair))
               (src-port (cadr pair)))
           (mk-wire src-name src-port app 'l)))
       outputs apps))

(define (chain-wires apps)
  (let loop ((xs apps) (acc '()))
    (if (null? (cdr xs)) (reverse acc)
        (let ((a (car xs)) (b (cadr xs)))
          (loop (cdr xs) (cons (mk-wire a 'r b 'p) acc))))))

(define (last-wire apps x-port)
  (let ((last (car (reverse apps))))
    (if (symbol? x-port)
        (mk-wire last 'r x-port 'p)
        (list 'wire (list last 'r) x-port))))

(define (out-wire apps out-target)
  (let ((first (car apps)))
    (if (symbol? out-target)
        (mk-wire first 'p out-target 'p)
        (list 'wire (list first 'p) out-target))))

(define (assemble-church-net apps copier-net app-forms LWs CWs lastW out-target outW)
  `(nu ,apps
       (par
        ,@(if copier-net (list copier-net) '())
        ,@app-forms
        ,@LWs
        ,@CWs
        ,lastW
        ,@(if (symbol? out-target) (list (mk-node out-target 'A 'user/output)) '())
        ,outW)))

(define (IC_CHURCH-APPLY n f-port x-port out-target)
  (ensure-number n "IC_CHURCH-APPLY")
  (cond
    ((<= n 0)
     (church-zero-net x-port out-target))
    (else
     (let* ((f-ep        (normalize-ep f-port 'p))
            (apps        (make-apps n))
            (app-forms   (app-node-forms apps))
            (fan         (build-copier-fanout f-ep n))
            (outputs     (car fan))
            (copier-net  (cdr fan))
            (LWs         (left-wires outputs apps))
            (CWs         (chain-wires apps))
            (lastW       (last-wire apps x-port))
            (outW        (out-wire  apps out-target)))
       (assemble-church-net apps copier-net app-forms LWs CWs lastW
                            out-target outW)))))



(define (IC_PRIM_ADD_RUNTIME in1 in2 out)
  (IC_PRIM_ADD in1 in2 out))

(define (IC_CONS head tail name)
  (let ((sym name))
    `(par
      ,(mk-node sym 'A 'ds/cons)
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
    (mk-nu (list sym e1 e2)
           (mk-par
            (mk-node sym 'A 'ds/nil)
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
          ,(if (symbol? list-port) (mk-wire list-port 'p fold-n 'l) (list 'wire list-port (list fold-n 'l)))
          ,(if (symbol? acc-port) (mk-wire acc-port 'p fold-n 'r) (list 'wire acc-port (list fold-n 'r)))
          ,(mk-node out 'A)))))


(define (IC_PURE_ID x-port out-node)
  (let ((id-impl (gensym "id-impl-"))
        (app      (gensym "app-")))
    `(nu (,id-impl ,app)
       (par
        ,(mk-node id-impl 'A)
        ,(mk-node app 'A)
        ,(mk-node out-node 'A)
        ,(wire-or-list x-port app 'l)
        ,(mk-wire id-impl 'p app 'r)
        ,(mk-wire app 'p out-node 'p)))))

(define (IC_PURE_PAIR a-port b-port pair-out-node)
  (let ((ap1 (gensym "pair-ap1-"))
        (ap2 (gensym "pair-ap2-"))
        (c-a (gensym "pair-ca-"))
        (c-b (gensym "pair-cb-")))
    `(nu (,ap1 ,ap2 ,c-a ,c-b)
       (par
        ,(mk-node pair-out-node 'A)
        ,(mk-node ap1 'A)
        ,(mk-node ap2 'A)
        ,(mk-node c-a 'C)
        ,(wire-or-list a-port c-a)
        ,(mk-node c-b 'C)
        ,(wire-or-list b-port c-b)
        ,(mk-wire pair-out-node 'l ap1 'l)
        ,(mk-wire c-a 'l ap1 'r)
        ,(mk-wire ap1 'p ap2 'l)
        ,(mk-wire c-b 'l ap2 'r)
        ,(mk-wire ap2 'p pair-out-node 'p)))))

(define (IC_PURE_FST pair-port out-node)
  (let ((K     (gensym "fst-K-"))
        (k-ap1 (gensym "fst-kap1-"))
        (k-ap2 (gensym "fst-kap2-"))
        (app   (gensym "fst-app-"))
        (drop  (gensym "fst-drop-")))
    `(nu (,K ,k-ap1 ,k-ap2 ,app ,drop)
       (par
        ,(mk-node out-node 'A)
        ,(mk-node K 'A)
        ,(mk-node k-ap1 'A)
        ,(mk-node k-ap2 'A)
        ,(mk-node drop 'E)
        ,(mk-wire K 'l k-ap2 'p)
        ,(mk-wire k-ap2 'l drop 'p)

        ,(mk-node app 'A)
        ,(wire-or-list (list (car pair-port) 'l) K 'p)
        ,(mk-wire K 'p app 'l)
        ,(mk-wire app 'p out-node 'p))))) 

(define (IC_PURE_SND pair-port out-node)
  (let ((Kp   (gensym "snd-Kp-"))
        (kp1  (gensym "snd-kp1-"))
        (kp2  (gensym "snd-kp2-"))
        (app  (gensym "snd-app-"))
        (drop (gensym "snd-drop-")))
    `(nu (,Kp ,kp1 ,kp2 ,app ,drop)
       (par
        ,(mk-node out-node 'A)
        ,(mk-node Kp 'A)
        ,(mk-node kp1 'A)
        ,(mk-node kp2 'A)
        ,(mk-node drop 'E)
        ,(mk-wire Kp 'l drop 'p)
        ,(mk-wire kp1 'l kp1 'p)
        ,(mk-node app 'A)
        ,(wire-or-list (list (car pair-port) 'l) Kp 'p)
        ,(mk-wire Kp 'p app 'l)
        ,(mk-wire app 'p out-node 'p))))) 

(define (IC_PURE_LEFT a-port left-out-node)
  (let ((c-a (gensym "left-ca-"))
        (app1 (gensym "left-app1-"))
        (f2 (gensym "left-f2-"))
        (drop (gensym "left-drop-")))
    `(nu (,c-a ,app1 ,f2 ,drop)
       (par
        ,(mk-node left-out-node 'A)
        ,(mk-node c-a 'C)
        ,(wire-or-list a-port c-a)
        ,(mk-node f2 'A)
        ,(mk-node drop 'E)
        ,(mk-wire f2 'l drop 'p)
        ,(mk-node app1 'A)
        ,(mk-wire left-out-node 'l app1 'l)
        ,(mk-wire c-a 'l app1 'r)
        ,(mk-wire app1 'p f2 'p)
        ,(mk-wire left-out-node 'r f2 'r)))))

(define (IC_PURE_RIGHT b-port right-out-node)
  (let ((c-b (gensym "right-cb-"))
        (app1 (gensym "right-app1-"))
        (f2 (gensym "right-f2-"))
        (drop (gensym "right-drop-")))
    `(nu (,c-b ,app1 ,f2 ,drop)
       (par
        ,(mk-node right-out-node 'A)
        ,(mk-node drop 'E)
        ,(mk-wire right-out-node 'l drop 'p)
        ,(mk-node c-b 'C)
        ,(wire-or-list b-port c-b)
        ,(mk-node f2 'A)
        ,(mk-node app1 'A)
        ,(mk-wire f2 'l app1 'l)
        ,(mk-wire c-b 'l app1 'r)
        ,(mk-wire app1 'p f2 'p)
        ,(mk-wire right-out-node 'r f2 'r)))))

(define (IC_PURE_EITHER v-port l-port r-port out-node)
  (let ((copy (gensym "either-copy-"))
        (app1 (gensym "either-app1-"))
        (app2 (gensym "either-app2-")))
    `(nu (,copy ,app1 ,app2)
         (par
          ,(mk-node out-node 'A)
          ,(mk-node copy 'C)
          ,(mk-node app1 'A)
          ,(mk-node app2 'A)
          ;; duplicate the input value
          ,(mk-wire (car v-port) 'p copy 'p)
          ,(mk-wire copy 'l app1 'l)
          ,(mk-wire copy 'r app2 'l)
          ;; connect left function
          ,(wire-or-list l-port app1 'r)
          ;; chain left app output into right app input
          ,(mk-wire app1 'p app2 'l)
          ;; connect right function
          ,(wire-or-list r-port app2 'r)
          ;; final output
          ,(mk-wire app2 'p out-node 'p)))))

