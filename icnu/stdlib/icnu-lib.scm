(define-module (icnu stdlib icnu-lib)
  #:use-module (icnu icnu)
  #:use-module (icnu utils compat)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils helpers)
  #:use-module (icnu utils internal)
  #:use-module (icnu stdlib ic-lib)
  #:export (ICNU_TRUE ICNU_FALSE ICNU_IF ICNU_Y
		                ICNU_JOIN-REPLACE ICNU_JOIN-MAX
		                ICNU_LITERAL ICNU_EQ_CONST ICNU_LT_CONST ICNU_GT_CONST
		                ICNU_AND ICNU_OR ICNU_NOT ICNU_COPY
		                ICNU_PRIM_ADD ICNU_PRIM_SUB ICNU_PRIM_SUM1 ICNU_PRIM_ADD_RUNTIME ICNU_APPLY
		                ICNU_MK_TRUE ICNU_MK_FALSE
                    ICNU_CHURCH-APPLY ICNU_CONS ICNU_NIL ICNU_FIRST ICNU_REST ICNU_FOLD
			              ICNU_PURE_ID ICNU_PURE_PAIR ICNU_PURE_FST ICNU_PURE_SND ICNU_PURE_LEFT ICNU_PURE_RIGHT
			              ICNU_PURE_EITHER
                    ICNU_GENERIC_CONST_OP
                    ))


(define (ICNU_TRUE b)
  `(par ,@(IC_TRUE b)))

(define (ICNU_FALSE b)
  `(par ,@(IC_FALSE b)))

(define (ICNU_IF c-port t-port e-port out-node)
  `(par
    ,@(IC_IF c-port t-port e-port out-node)))


(define (ICNU_JOIN-REPLACE x y out)
  `(par ,@(JOIN-REPLACE x y out)))

(define (ICNU_JOIN-MAX x y out)
  `(par ,@(JOIN-MAX x y out)))

(define (ICNU_LITERAL val out)
  (cond
   ((char? val)
    `(par (node ,out A lit/char ,val)))
   ((and (number? val) (integer? val))
    `(par (node ,out A lit/int ,val)))
   ((and (number? val) (real? val))
    `(par (node ,out A lit/real ,val)))
   ((number? val)
    `(par (node ,out A lit/num ,val)))
   ((string? val)
    `(par (node ,out A lit/str ,val)))
   ((symbol? val)
    `(par (node ,out A lit/symbol ',val)))
   (else
    (error "ICNU_LITERAL: unsupported literal type (bool/pair moved to pure IC)" val))))

(define (ICNU_APPLY f-port x-port out)
  `(par ,@(IC_APPLY f-port x-port out)))

(define (ICNU_GENERIC_CONST_OP const-val in-port out-node tag)
  (let ((lit (icnu-gensym "lit-")))
    `(par
      ,(ICNU_LITERAL const-val lit)
      ,@(IC_GENERIC_CONST_OP const-val in-port out-node tag)
      (wire (,lit p) (,out-node r)))))

(define (ICNU_EQ_CONST const-val in-port out-node)
  (ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/eq))

(define (ICNU_LT_CONST const-val in-port out-node)
  (ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/lt))

(define (ICNU_GT_CONST const-val in-port out-node)
  (ICNU_GENERIC_CONST_OP const-val in-port out-node 'prim/gt))

(define (ICNU_COPY in out)
  `(par ,@(IC_COPY in out)))

(define (ICNU_AND in1-port in2-port out-node)
  `(par
    (node ,out-node A)
    ,(ICNU_IF in1-port in2-port in1-port out-node)))

(define (ICNU_OR in1-port in2-port out-node)
  `(par
    (node ,out-node A)
    ,(ICNU_IF in1-port in1-port in2-port out-node)))

(define (ICNU_NOT in-port out-node)
  (let ((true-node (icnu-gensym "true-"))
        (false-node (icnu-gensym "false-")))
    `(nu (,true-node ,false-node)
         (par
          (node ,out-node A)
          ,(ICNU_MK_TRUE true-node)
          ,(ICNU_MK_FALSE false-node)
          ,(ICNU_IF in-port (list false-node 'r) (list true-node 'r) out-node)))))

(define (ICNU_PRIM_ADD in1 in2 out)
  `(par ,@(IC_PRIM_ADD in1 in2 out)))

(define (ICNU_PRIM_SUB in1 in2 out)
  `(par ,@(IC_PRIM_SUB in1 in2 out)))

(define (ICNU_PRIM_SUM1 in out)
  `(par ,@(IC_PRIM_SUM1 in out)))

(define (ICNU_MK_TRUE b)
  `(par ,@(IC_TRUE b)))

(define (ICNU_MK_FALSE b)
  `(par ,@(IC_FALSE b)))

(define (ICNU_Y fn out)
  (IC_Y fn out))


(define (ICNU_CHURCH-APPLY n f-port x-port out-target)
  (IC_CHURCH-APPLY n f-port x-port out-target))



(define (ICNU_PRIM_ADD_RUNTIME in1 in2 out)
  (ICNU_PRIM_ADD in1 in2 out))

(define (ICNU_CONS head tail name)
  (cond
   ((and (list? head) (eq? (car head) 'quote))
    (let ((lit-name (icnu-gensym "cons-lit-")))
      `(par
        ,(ICNU_LITERAL (cadr head) lit-name)
        ,@(IC_CONS (list lit-name 'p) tail name))))
   ((symbol? head)
    `(par ,@(IC_CONS (list head 'p) tail name)))
   (else
    `(par ,@(IC_CONS head tail name)))))

(define (ICNU_NIL name)
  `(par ,@(IC_NIL name)))

(define (ICNU_FIRST cons out)
  `(par ,@(IC_FIRST cons out)))

(define (ICNU_REST cons out)
  `(par ,@(IC_REST cons out)))

(define (ICNU_FOLD list-port acc-port fn-port out)
  `(par ,@(IC_FOLD list-port acc-port fn-port out)))


(define (ICNU_PURE_ID x-port out-node)
  `(par ,@(IC_PURE_ID x-port out-node)))

(define (ICNU_PURE_PAIR a-port b-port pair-out-node)
  `(par ,@(IC_PURE_PAIR a-port b-port pair-out-node)))

(define (ICNU_PURE_FST pair-port out-node)
  `(par ,@(IC_PURE_FST pair-port out-node)))

(define (ICNU_PURE_SND pair-port out-node)
  `(par ,@(IC_PURE_SND pair-port out-node)))


(define (ICNU_PURE_LEFT a-port left-out-node)
  `(par ,@(IC_PURE_LEFT a-port left-out-node)))

(define (ICNU_PURE_RIGHT b-port right-out-node)
  `(par ,@(IC_PURE_RIGHT b-port right-out-node)))

(define (ICNU_PURE_EITHER v-port l-port r-port out-node)
  `(par ,@(IC_PURE_EITHER v-port l-port r-port out-node)))


