(define-module (icnu eval)
  #:use-module (icnu utils internal)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu tools icnu-validate)
  #:use-module (icnu stdlib ic-lib)
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (icnu stdlib unit)
  #:use-module (ice-9 match)
  #:export (eval-icnu-string eval-net reduce-net-to-normal-form *default-reduction-passes* ic-only-reduction-passes read-sexpr-from-string
                             parse-icnu-string-to-net
                             find-applicator-for-target))

(define (read-sexpr-from-string s)
  (call-with-input-string s read))


(define l2-fns
  (list
   ;; ic-lib (순수 IC)
   (cons 'IC_PRIM_ADD IC_PRIM_ADD)
   (cons 'IC_PRIM_SUB IC_PRIM_SUB)
   (cons 'IC_PRIM_SUM1 IC_PRIM_SUM1)
   (cons 'IC_APPLY IC_APPLY)
   (cons 'IC_CONS IC_CONS)
   (cons 'IC_NIL IC_NIL)
   (cons 'IC_FIRST IC_FIRST)
   (cons 'IC_REST IC_REST)
   (cons 'IC_FOLD IC_FOLD)
   (cons 'IC_PURE_ID IC_PURE_ID)
   (cons 'IC_PURE_PAIR IC_PURE_PAIR)
   (cons 'IC_PURE_FST IC_PURE_FST)
   (cons 'IC_PURE_SND IC_PURE_SND)
   (cons 'IC_PURE_LEFT IC_PURE_LEFT)
   (cons 'IC_PURE_RIGHT IC_PURE_RIGHT)
   (cons 'IC_PURE_EITHER IC_PURE_EITHER)
   (cons 'IC_IF IC_IF)
   (cons 'IC_Y IC_Y)
   ;; icnu-lib (비순수 확장, 하지만 여기서는 S-표현식 생성만)
   (cons 'ICNU_PRIM_ADD ICNU_PRIM_ADD)
   (cons 'ICNU_PRIM_SUB ICNU_PRIM_SUB)
   (cons 'ICNU_PRIM_SUM1 ICNU_PRIM_SUM1)
   (cons 'ICNU_APPLY ICNU_APPLY)
   (cons 'ICNU_CONS ICNU_CONS)
   (cons 'ICNU_NIL ICNU_NIL)
   (cons 'ICNU_FIRST ICNU_FIRST)
   (cons 'ICNU_REST ICNU_REST)
   (cons 'ICNU_FOLD ICNU_FOLD)
   (cons 'ICNU_PURE_ID ICNU_PURE_ID)
   (cons 'ICNU_PURE_PAIR ICNU_PURE_PAIR)
   (cons 'ICNU_PURE_FST ICNU_PURE_FST)
   (cons 'ICNU_PURE_SND ICNU_PURE_SND)
   (cons 'ICNU_PURE_LEFT ICNU_PURE_LEFT)
   (cons 'ICNU_PURE_RIGHT ICNU_PURE_RIGHT)
   (cons 'ICNU_PURE_EITHER ICNU_PURE_EITHER)
   (cons 'ICNU_IF ICNU_IF)
   (cons 'ICNU_Y ICNU_Y)
   (cons 'ICNU_LITERAL ICNU_LITERAL)
   (cons 'ICNU_EQ_CONST ICNU_EQ_CONST)
   (cons 'ICNU_LT_CONST ICNU_LT_CONST)
   (cons 'ICNU_GT_CONST ICNU_GT_CONST)
   ;; unit
   (cons 'IC_UNIT IC_UNIT)
   (cons 'IC_CALL_UNIT IC_CALL_UNIT)))

(define (lookup-l2-fn sym)
  (let ((p (assq sym l2-fns)))
    (and p (cdr p))))

(define (to-forms x)
  (cond
   ((and (pair? x) (eq? (car x) 'par)) (cdr x))
   ((and (pair? x) (not (symbol? (car x)))) x)
   (else (list x))))

(define (expand-call-form form)
  (match form
    (('call fname . args)
     (let ((fn (and (symbol? fname) (lookup-l2-fn fname))))
       (if fn
           (to-forms (apply fn args))
           (error "L2: unknown function in (call ...)" fname))))
    (_ (list form))))

(define (expand-par-body forms)
  (letrec ((expand-form
            (lambda (f)
              (match f
                (('par . xs)
                 (apply append (map expand-form xs)))
                (('call fname . args)
                 (let ((fn (and (symbol? fname) (lookup-l2-fn fname))))
                   (if fn
                       (let ((expanded (to-forms (apply fn args))))
                         (apply append (map expand-form expanded)))
                       (error "L2: unknown function in (call ...)" fname))))
                (_ (list f))))))
    (apply append (map expand-form forms))))

(define (expand-module sexpr)
  ;; (module (import ...) body...) OR (module import-decls body...)
  ;; body가 여러 폼이면 모두 평탄화하여 하나의 (par ...)로 결합
  (match sexpr
    (('module import-or-decls . bodies)
     (let* ((to-body-forms
             (lambda (b)
               (if (and (pair? b) (eq? (car b) 'par)) (cdr b) (list b))))
            (body-forms (apply append (map to-body-forms bodies)))
            (expanded (expand-par-body body-forms)))
       `(par ,@expanded)))
    (_ (error "L2: malformed module form" sexpr))))

(define (maybe-eval-layer2 sexpr)
  (cond
   ((and (pair? sexpr) (eq? (car sexpr) 'module))
    (expand-module sexpr))
   ((and (pair? sexpr) (eq? (car sexpr) 'l2))
    (error "L2: (l2 ...) form is deprecated. Use (module (import ...) (par ... (call FN ...) ...))"))
   (else sexpr)))

(define (parse-icnu-string-to-net icnu-string . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr0 (read-sexpr-from-string icnu-string))
         (sexpr (maybe-eval-layer2 sexpr0))
         (use-nu? (opt-ref opts 'use-nu? #t)))
    (parse-net sexpr use-nu?)))

(define *default-reduction-passes*
  (lambda ()
    (list rewrite-pass-const-fold!
          rewrite-pass-if-fold!
          rewrite-pass-AA-merge!
          rewrite-pass-AC!
          rewrite-pass-inpack-direct-wire!
          rewrite-pass-AE!
          rewrite-pass-CE-annihilation!
          rewrite-pass-wire-cleanup!)))

(define (ic-only-reduction-passes)
  (list rewrite-pass-AA-merge!
        rewrite-pass-AC!
        rewrite-pass-inpack-direct-wire!
        rewrite-pass-AE!
        rewrite-pass-CE-annihilation!
        rewrite-pass-wire-cleanup!))

(define (apply-reduction-passes! net passes)
  (let ((changed? #f))
    (for-each
     (lambda (pass)
       (when (pass net) (set! changed? #t)))
     passes)
    changed?))

(define (reduce-net-to-normal-form net . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (max-entry (assoc 'max-iter opts))
         (max-iter (if max-entry (cdr max-entry) 100))
         (passes (let ((v (assq-ref opts 'passes)))
                   (cond
                    ((not v) (*default-reduction-passes*))
                    ((procedure? v) (v))
                    ((symbol? v)
                     (let ((proc (eval v (current-module))))
                       (if (procedure? proc) (proc) (*default-reduction-passes*))))
                    ((and (list? v) (list? (car v))
                          (icnu-andmap (lambda (x) (or (procedure? x) (symbol? x))) (car v)))
                     (map (lambda (p) (if (procedure? p) p (eval p (current-module)))) (car v)))
                    ((list? v)
                     (map (lambda (p) (if (procedure? p) p (eval p (current-module)))) v))
                    (else (*default-reduction-passes*))))))
    (let loop ((i 0))
      (let ((errors (validate-ir net)))
        (when (not (null? errors))
          (error "reduce-net-to-normal-form: validation failed at step" i errors (pretty-print net '((show-nu? . #t))))))
      (if (and (number? max-iter) (> i max-iter))
          (begin
            (warnf "reduce-net-to-normal-form: exceeded max iterations\n")
            net)
          (if (apply-reduction-passes! net passes)
              (loop (+ i 1))
              net)))
    net))

(define (opt-ref opts key default)
  (let ((v (assq-ref opts key)))
    (if v v default)))

(define (common-ports) '(p r l))

(define (ports-excluding primary)
  (icnu-filter (lambda (p) (not (eq? p primary))) '(r l p)))

(define (resolve-ep->literal net ep)
  (let ((res (resolve-literal-ep net ep)))
    (if (not (eq? res *unresolved*)) res *unresolved*)))

(define (resolve-peer-general-or-direct net peer-ep)
  (if (not peer-ep)
      *unresolved*
      (let* ((pn (car peer-ep))
             (res (resolve-literal-ep net peer-ep)))
        (cond
         ((not (eq? res *unresolved*)) res)
         ((and pn (is-literal-node? net pn)) (get-literal-value net pn))
         (else *unresolved*)))))


(define (try-primary-port net out-name out-port)
  (resolve-ep->literal net (endpoint out-name out-port)))

(define (try-other-ports net out-name out-port)
  (let loop ((ps (ports-excluding out-port)))
    (if (null? ps)
        *unresolved*
        (let ((res (resolve-ep->literal net (endpoint out-name (car ps)))))
          (if (not (eq? res *unresolved*)) res (loop (cdr ps)))))))

(define (final-fallback net out-name)
  (let loop ((ps (common-ports)))
    (if (null? ps)
        *unresolved*
        (let* ((pp (peer net (endpoint out-name (car ps))))
               (res (resolve-peer-general-or-direct net pp)))
          (if (not (eq? res *unresolved*))
              res
              (loop (cdr ps)))))))


(define (find-applicator-for-target net target-ep)
  (let ((found #f))
    (hash-for-each
     (lambda (name agent)
       (when (and (not found) (eq? agent 'A))
         (let ((l-peer (peer net (cons name 'l))))
           (when l-peer
             (let ((l-val (resolve-literal-ep net l-peer)))
               (when (equal? l-val target-ep)
                 (set! found name)))))))
     (net-nodes net))
    found))

(define (resolve-from-out-name net out-name out-port opts)
  (let ((primary (try-primary-port net out-name out-port)))
    (if (not (eq? primary *unresolved*))
        primary
        (let ((other (try-other-ports net out-name out-port)))
          (if (not (eq? other *unresolved*))
              other
              ;; Quick heuristic: if the requested out port peers to a copier's r-port,
              ;; and that copier's principal port is linked to some endpoint, expose
              ;; that endpoint (or its literal) immediately. This covers common patterns
              ;; created by IC_CONS + IC_FIRST where the copier's r carries the value endpoint.
              (let ((direct-peer (peer net (endpoint out-name out-port))))
                (let* ((copier-target
                        (and direct-peer
                             (pair? direct-peer)
                             (memq (cdr direct-peer) '(l r))
                             (eq? (node-agent net (car direct-peer)) 'C)
                             (peer net (cons (car direct-peer) 'p))))
                       (a-prop-target
                        (and (not copier-target)
                             direct-peer
                             (pair? direct-peer)
                             (eq? (cdr direct-peer) 'p)
                             (eq? (node-agent net (car direct-peer)) 'A)
                             (let* ((an (car direct-peer))
                                    (try-side
                                     (lambda (side)
                                       (let ((lep (peer net (endpoint an side))))
                                         (and lep
                                              (pair? lep)
                                              (memq (cdr lep) '(l r))
                                              (eq? (node-agent net (car lep)) 'C)
                                              (peer net (cons (car lep) 'p))))))
                                    (cand-l (try-side 'l))
                                    (cand-r (try-side 'r)))
                               (or cand-l cand-r))))
                       (target (or copier-target a-prop-target)))
                  (if target
                      (let ((resolved (resolve-literal-ep net target *resolve-literal-limit*)))
                        (if (not (eq? resolved *unresolved*))
                            resolved
                            (let* ((target-ep (assq-ref opts 'applicator-target))
                                   (app-node (and target-ep (find-applicator-for-target net target-ep))))
                              (if app-node
                                  (let ((res2 (resolve-literal-ep net (endpoint app-node 'p) *resolve-literal-limit*)))
                                    (if (not (eq? res2 *unresolved*)) res2 (final-fallback net out-name)))
                                  (final-fallback net out-name)))))
                      (final-fallback net out-name)))))))))

(define (extract-result-from-net net opts)
  (let* ((out-name    (assq-ref opts 'out-name))
         (out-port    (opt-ref opts 'out-port 'p))
         (result-form (opt-ref opts 'result-form 'string)))
    (cond
     (out-name
      (resolve-from-out-name net out-name out-port opts))
     ((eq? result-form 'net)
      net)
     (else
      (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))))
  )

(define (eval-net net . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (reduced-net (reduce-net-to-normal-form net opts)))
    (extract-result-from-net reduced-net opts)))

(define (eval-icnu-string icnu-string . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr0 (read-sexpr-from-string icnu-string))
         (sexpr (maybe-eval-layer2 sexpr0))
         (net (parse-net sexpr (opt-ref opts 'use-nu? #t))))
    (eval-net net opts)))

