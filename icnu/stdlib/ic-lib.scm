(define-module (icnu stdlib ic-lib)
  #:use-module (icnu ic)
  #:use-module (icnu utils compat)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils helpers)
  #:use-module (icnu utils internal)
  #:export (IC_IF IC_Y
		              JOIN-REPLACE JOIN-MAX
		              IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		              IC_AND IC_OR IC_NOT IC_COPY
		              IC_PRIM_ADD IC_PRIM_ADD_RUNTIME IC_APPLY
		              IC_MK_TRUE IC_MK_FALSE
                  IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD
			            IC_PURE_ID IC_PURE_PAIR IC_PURE_FST IC_PURE_SND IC_PURE_LEFT IC_PURE_RIGHT
			            IC_PURE_EITHER))

(define (IC_IF c-port t-port e-port out-node)
  (let* ((if-impl (icnu-gensym "if-impl-"))
         (c-c (icnu-gensym "cond-copy-"))
         (t-c (icnu-gensym "then-copy-"))
         (e-c (icnu-gensym "else-copy-")))
    `(par
      (node ,out-node A user/output "IF result output")
      (node ,if-impl A prim/if "Core IF agent")
      (node ,c-c C)
      ,(wire-or-list c-port c-c)
      (wire (,c-c l) (,if-impl p))
      (node ,t-c C)
      ,(wire-or-list t-port t-c)
      (wire (,t-c l) (,if-impl l))
      (node ,e-c C)
      ,(wire-or-list e-port e-c)
      (wire (,e-c l) (,if-impl r))
      (wire (,c-c r) (,out-node p)))))


(define (JOIN-REPLACE x y out)
  (let ((e (icnu-gensym "erase_")))
    `(par 
      (node ,e E)
      (wire (,e p) (,x p))
      (wire (,out p) (,y p)))))

(define (JOIN-MAX x y out)
  (let ((comp (icnu-gensym "max_comp_")))
    `(par 
      (node ,comp A)
      (wire (,comp p) (,out p)))))

(define (IC_LITERAL val out)
  (cond
   ((boolean? val)
    `(par (node ,out A lit/bool ,val)))
   ((number? val)
    `(par (node ,out A lit/num ,val)))
   ((or (symbol? val) (string? val))
    `(par (node ,out A lit/str ,(if (symbol? val) `',val val))))
   (else
    (error "IC_LITERAL: unsupported literal type" val))))

(define (IC_APPLY f-port x-port out)
  (let* ((out-node (if (symbol? out) out out))
         (c-f (icnu-gensym "app-f-"))
         (c-x (icnu-gensym "app-x-")))
    `(par
      (node ,out-node A user/apply-out "APPLY output node")
      (node ,c-f C)
      (node ,c-x C)
      ,(wire-or-list f-port c-f)
      ,(wire-or-list x-port c-x)
      (wire (,c-f l) (,out-node l))
      (wire (,c-x l) (,out-node r)))))

(define (IC_GENERIC_CONST_OP const-val in-port out-node tag)
  (let* ((lit (icnu-gensym "lit-"))
         (in-copy (icnu-gensym "in-copy-")))
    `(par 
      ,(IC_LITERAL const-val lit)
      (node ,out-node A ,tag)
      (node ,in-copy C)
      ,(wire-or-list in-port in-copy)
      (wire (,in-copy l) (,out-node l))
      (wire (,lit p) (,out-node r)))))

(define (IC_EQ_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/eq))

(define (IC_LT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/lt))

(define (IC_GT_CONST const-val in-port out-node)
  (IC_GENERIC_CONST_OP const-val in-port out-node 'prim/gt))

(define (IC_COPY in out)
  (if (eq? in out)
      `(par (node ,out A))
      (let ((c-left (icnu-gensym "c-left-"))
            (c-right (icnu-gensym "c-right-")))
        `(par
          (node ,out A user/copier-out "COPY output node")
          (node ,c-left C)
          (wire (,in l) (,c-left p))
          (wire (,c-left l) (,out l))
          (node ,c-right C)
          (wire (,in r) (,c-right p))
          (wire (,c-right l) (,out r))))))

(define (IC_AND in1-port in2-port out-node)
  `(par
    (node ,out-node A)
    ,(IC_IF in1-port in2-port in1-port out-node)))

(define (IC_OR in1-port in2-port out-node)
  `(par
    (node ,out-node A)
    ,(IC_IF in1-port in1-port in2-port out-node)))

(define (IC_NOT in-port out-node)
  (let ((true-node (icnu-gensym "true-"))
        (false-node (icnu-gensym "false-")))
    `(par
      (node ,out-node A)
      ,(IC_LITERAL #t true-node)
      ,(IC_LITERAL #f false-node)
      ,(IC_IF in-port (list false-node 'r) (list true-node 'r) out-node))))

(define (IC_PRIM_ADD in1 in2 out)
  (let ((add-impl (icnu-gensym "add-"))
        (c1 (icnu-gensym "c-")) (c2 (icnu-gensym "c-"))
        (out-c (icnu-gensym "out-copy-")))
    `(par
      (node ,add-impl A prim/add "Core ADD agent")
      (node ,c1 C)
      (node ,c2 C)
      ,(wire-or-list in1 c1)
      ,(wire-or-list in2 c2)
      (wire (,c1 l) (,add-impl l))
      (wire (,c2 l) (,add-impl r))
      (node ,out A)
      (node ,out-c C)
      (wire (,add-impl p) (,out-c p))
      (wire (,out-c l) (,out r)))))

(define (IC_MK_TRUE b)
  `(par (node ,b A lit/bool #t)))

(define (IC_MK_FALSE b)
  `(par (node ,b A lit/bool #f)))

(define (IC_Y fn out)
  (let* ((y-node   (icnu-gensym "Y"))
         (dup-node (icnu-gensym "Ydup"))
         (app1     (icnu-gensym "Yapp1"))
         (app2     (icnu-gensym "Yapp2"))
         (res-node (icnu-gensym "Yres")))
    `(par
      (node ,y-node A y-comb "Y combinator structure")
      (node ,dup-node C)
      (node ,app1 A y-comb "Y combinator app1")
      (node ,app2 A y-comb "Y combinator app2")
      (node ,res-node A y-comb "Y combinator result output")
      ,(wire-or-list fn dup-node)
      (wire (,dup-node l) (,app1 p))
      (wire (,dup-node r) (,app2 p))
      (wire (,app1 l) (,y-node l))
      (wire (,app1 r) (,app2 l))
      (wire (,app2 r) (,y-node r))
      (wire (,y-node p) (,res-node l))
      (wire (,res-node p) (,out p)))))


(define (church-zero-net x-port out-target)
  (let ((from-ep (icnu-normalize-ep x-port 'p))
        (to-ep   (icnu-normalize-ep out-target 'p)))
    `(par ,@(if (symbol? out-target) `((node ,out-target A)) '())
          (wire ,from-ep ,to-ep))))

(define (build-copier-fanout input-ep k)
  (letrec ((go (lambda (in k)
                 (if (<= k 1)
                     (cons (list in) #f)
                     (let* ((c (icnu-gensym "copier-"))
                            (nl (ceiling (/ k 2)))
                            (nr (floor   (/ k 2)))
                            (L (go (list c 'l) nl))
                            (R (go (list c 'r) nr))
                            (outs (append (car L) (car R)))
                            (netL (cdr L)) (netR (cdr R))
                            (kids (icnu-filter (lambda (x) x) (list netL netR))))
                       (cons outs
                             `(par
                               (node ,c C)
                               ,(wire-or-list in c)
                               ,@kids)))))))
    (go input-ep k)))

(define (make-apps n)
  (icnu-gensyms "church-app-" n))

(define (app-node-forms apps)
  (map (lambda (nm) `(node ,nm A)) apps))

(define (left-wires outputs apps)
  (map (lambda (pair app)
         (let ((src-name (car pair))
               (src-port (cadr pair)))
           `(wire (,src-name ,src-port) (,app l))))
       outputs apps))

(define (chain-wires apps)
  (let loop ((xs apps) (acc '()))
    (if (null? (cdr xs)) (reverse acc)
        (let ((a (car xs)) (b (cadr xs)))
          (loop (cdr xs) (cons `(wire (,a r) (,b p)) acc))))))

(define (last-wire apps x-port)
  (let ((last (car (reverse apps))))
    (if (symbol? x-port)
        `(wire (,last r) (,x-port p))
        `(wire (,last r) ,x-port))))

(define (out-wire apps out-target)
  (let ((first (car apps)))
    (if (symbol? out-target)
        `(wire (,first p) (,out-target p))
        `(wire (,first p) ,out-target))))

(define (assemble-church-net apps copier-net app-forms LWs CWs lastW out-target outW)
  `(par
    ,@(if copier-net (list copier-net) '())
    ,@app-forms
    ,@LWs
    ,@CWs
    ,lastW
    ,@(if (symbol? out-target) `((node ,out-target A user/output)) '())
    ,outW))

(define (IC_CHURCH-APPLY n f-port x-port out-target)
  (icnu-ensure-number n "IC_CHURCH-APPLY")
  (cond
   ((<= n 0)
    (church-zero-net x-port out-target))
   (else
    (let* ((f-ep        (icnu-normalize-ep f-port 'p))
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
      (node ,sym A ds/cons "CONS cell")
      ,(if (symbol? head)
           `(wire (,head p) (,sym l))
           `(wire ,head (,sym l)))
      ,(if (symbol? tail)
           `(wire (,tail p) (,sym r))
           `(wire ,tail (,sym r))))))

(define (IC_NIL name)
  (let ((sym (if (symbol? name) name (string->symbol (format-string #f "nil-~a" name))))
        (e1 (icnu-gensym "e_"))
        (e2 (icnu-gensym "e_")))
    `(par
      (node ,sym A ds/nil "NIL cell")
      (node ,e1 E)
      (node ,e2 E)
      (wire (,sym l) (,e1 p))
      (wire (,sym r) (,e2 p)))))

(define (IC_FIRST cons out)
  `(par (node ,out A) (wire (,cons l) (,out p))))

(define (IC_REST cons out)
  `(par (node ,out A) (wire (,cons r) (,out p))))

(define (IC_FOLD list-port acc-port fn-port out)
  (let ((fold-n (icnu-gensym "fold-")))
    `(par
      (node ,fold-n A)
      ,(if (symbol? list-port) `(wire (,list-port p) (,fold-n l)) `(wire ,list-port (,fold-n l)))
      ,(if (symbol? acc-port) `(wire (,acc-port p) (,fold-n r)) `(wire ,acc-port (,fold-n r)))
      (node ,out A))))


(define (IC_PURE_ID x-port out-node)
  `(par
    (node ,out-node A)
    ,(wire-or-list x-port out-node)))

(define (IC_PURE_PAIR a-port b-port pair-out-node)
  `(par
    (node ,pair-out-node A)
    ,(wire-or-list a-port pair-out-node 'l)
    ,(wire-or-list b-port pair-out-node 'r)))

(define (IC_PURE_FST pair-port out-node)
  (let ((fst-agent (icnu-gensym "fst-agent-"))
        (e (icnu-gensym "fst-eraser-")))
    `(par
       (node ,out-node A)
       (node ,fst-agent A)
       (node ,e E)
       ,(wire-or-list pair-port fst-agent 'p)
       (wire (,fst-agent l) (,out-node p))
       (wire (,fst-agent r) (,e p)))))

(define (IC_PURE_SND pair-port out-node)
  (let ((snd-agent (icnu-gensym "snd-agent-"))
        (e (icnu-gensym "snd-eraser-")))
    `(par
       (node ,out-node A)
       (node ,snd-agent A)
       (node ,e E)
       ,(wire-or-list pair-port snd-agent 'p)
       (wire (,snd-agent r) (,out-node p))
       (wire (,snd-agent l) (,e p)))))

(define (IC_PURE_LEFT a-port left-out-node)
  (let ((c-a (icnu-gensym "left-ca-"))
        (app1 (icnu-gensym "left-app1-"))
        (f2 (icnu-gensym "left-f2-"))
        (drop (icnu-gensym "left-drop-")))
    `(par
      (node ,left-out-node A)
      (node ,c-a C)
      ,(wire-or-list a-port c-a)
      (node ,f2 A)
      (node ,drop E)
      (wire (,f2 l) (,drop p))
      (node ,app1 A)
      (wire (,left-out-node l) (,app1 l))
      (wire (,c-a l) (,app1 r))
      (wire (,app1 p) (,f2 p))
      (wire (,left-out-node r) (,f2 r)))))

(define (IC_PURE_RIGHT b-port right-out-node)
  (let ((c-b (icnu-gensym "right-cb-"))
        (app1 (icnu-gensym "right-app1-"))
        (f2 (icnu-gensym "right-f2-"))
        (drop (icnu-gensym "right-drop-")))
    `(par
      (node ,right-out-node A)
      (node ,drop E)
      (wire (,right-out-node l) (,drop p))
      (node ,c-b C)
      ,(wire-or-list b-port c-b)
      (node ,f2 A)
      (node ,app1 A)
      (wire (,f2 l) (,app1 l))
      (wire (,c-b l) (,app1 r))
      (wire (,app1 p) (,f2 p))
      (wire (,right-out-node r) (,f2 r)))))

(define (IC_PURE_EITHER v-port l-port r-port out-node)
  (let ((app1 (icnu-gensym "either-app1-"))
        (app2 (icnu-gensym "either-app2-")))
    `(par
      (node ,out-node A)
      (node ,app1 A)
      (node ,app2 A)
      ,(wire-or-list v-port app1 'l)
      ,(wire-or-list l-port app1 'r)
      (wire (,app1 p) (,app2 l))
      ,(wire-or-list r-port app2 'r)
      (wire (,app2 p) (,out-node p)))))

