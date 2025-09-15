(define-module (icnu stdlib unit)
  #:use-module (icnu stdlib ic-lib)
  #:use-module (icnu utils helpers)
  #:use-module (icnu utils format)
  #:use-module (icnu utils internal)
  #:use-module (icnu utils compat)
  #:export (IC_UNIT IC_CALL_UNIT))

(define (IC_UNIT name in-names out-names ret-name body)
  (let* ((body-forms (if (and (pair? body) (eq? (car body) 'par)) (cdr body) (if body (list body) '())))
         (frame-node (string->symbol (format-string #f "~a-frame" name)))
         (in-pack-node 'in-pack)
         (out-pack-node 'out-pack)
         (unpack-forms
           `(
             ,@(IC_PURE_FST (list frame-node 'l) in-pack-node)
             ,@(IC_PURE_SND (list frame-node 'l) out-pack-node)))
         (all-body-forms (append unpack-forms body-forms))
         (collect
           (lambda (forms)
             (letrec ((walk
                       (lambda (f acc)
                         (cond
                           ((null? f) acc)
                           ((and (pair? f) (eq? (car f) 'node) (symbol? (cadr f)))
                            (walk (cdr f) (cons (cadr f) acc)))
                           ((pair? f)
                            (let ((acc2 (walk (car f) acc)))
                              (walk (cdr f) acc2)))
                           (else acc)))))
               (walk forms '()))))
         (body-decls (collect body-forms))
         (auto-decls (list in-pack-node out-pack-node))
         (raw (append (list ret-name) body-decls auto-decls))
         (uniq
           (lambda (lst)
             (let loop ((l lst) (acc '()))
               (if (null? l) (reverse acc)
                   (let ((x (car l)))
                     (if (memq x acc) (loop (cdr l) acc) (loop (cdr l) (cons x acc))))))))
         (internal-candidates (uniq raw))
         (internal (icnu-filter
                    (lambda (x) (and (symbol? x) (not (eq? x name))))
                    internal-candidates))
         (forms
           (list
            `(node ,name C)
            `(nu (,@internal)
                 (par
                   ,@all-body-forms
                   (node ,ret-name C)))
            `(wire (,ret-name l) (,name p))))) 
    forms))

(define (IC_CALL_UNIT unit-name in-pack out-pack result)
  (let ((frame (string->symbol (format-string #f "~a-frame" unit-name))))
    (let* ((in-ep  (if (symbol? in-pack)  `(,in-pack p)  in-pack))
           (out-ep (if (symbol? out-pack) `(,out-pack p) out-pack)))
      (append
       (list `(wire (,unit-name l) (,result p)))
       (IC_PURE_PAIR in-ep out-ep frame)
       (list `(wire (,frame l) (,unit-name r)))))))
