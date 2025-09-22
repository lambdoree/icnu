(define-module (icnu eval)
  #:use-module (icnu utils internal)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu tools icnu-validate)
  #:use-module (ice-9 match)
  #:export (eval-icnu-string eval-icnu-file eval-net reduce-net-to-normal-form *default-reduction-passes* ic-only-reduction-passes read-sexpr-from-string read-sexpr-from-file
                             parse-icnu-string-to-net parse-icnu-file-to-net))

(define (read-sexpr-from-string s)
  (call-with-input-string s read))

(define (read-sexpr-from-file path)
  (call-with-input-file path (lambda (port) (read port))))

;; simple path helpers
(define (dirname path)
  (let* ((n (string-length path)))
    (let loop ((i (- n 1)))
      (if (< i 0) "."
          (if (char=? (string-ref path i) #\/)
              (if (= i 0) "/" (substring path 0 i))
              (loop (- i 1)))))))

(define (path-absolute? p)
  (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)))

(define (path-join base rel)
  (cond
   ((path-absolute? rel) rel)
   ((or (string=? base "") (string=? base ".")) rel)
   ((string=? base "/") (string-append "/" rel))
   (else (string-append base "/" rel))))

;; component expansion + optional renaming
(define (collect-node-names sexpr)
  (let ((acc '()))
    (letrec ((go (lambda (x)
                   (cond
                    ((pair? x)
                     (match x
                       (('node name . rest)
                        (when (symbol? name) (set! acc (cons name acc)))
                        (go (cdr x)))
                       (else (begin (go (car x)) (go (cdr x))))))
                    (else #t)))))
      (go sexpr))
    (reverse acc)))

(define (make-rename-map names prefix)
  (map (lambda (nm)
         (cons nm (string->symbol (string-append prefix (symbol->string nm)))))
       names))

(define (rename-with-map sexpr rmap)
  (letrec ((rw (lambda (x)
                 (cond
                  ((symbol? x)
                   (let ((p (assq x rmap))) (if p (cdr p) x)))
                  ((pair? x)
                   (cons (rw (car x)) (rw (cdr x))))
                  (else x)))))
    (rw sexpr)))

(define (expand-components sexpr base-dir)
  (letrec ((resolve-path
            (lambda (cur-base p)
              (let ((sp (if (string? p) p (format-string #f "~a" p))))
                (if (icnu-string-prefix? "icnu/" sp)
                    sp
                    (path-join cur-base sp)))))
           (expand1
            (lambda (x cur-base)
              (match x
                (('component path)
                 (let* ((p (if (string? path) path (format-string #f "~a" path)))
                        (full (resolve-path cur-base p))
                        (sexpr0 (read-sexpr-from-file full))
                        (expanded (expand1 sexpr0 (dirname full))))
                   expanded))
                (('component path pref-arg)
                 (let* ((p (if (string? path) path (format-string #f "~a" path)))
                        (prefix (cond
                                 ((and (pair? pref-arg) (eq? (car pref-arg) 'prefix)) (cadr pref-arg))
                                 ((string? pref-arg) pref-arg)
                                 (else (format-string #f "~a" pref-arg))))
                        (full (resolve-path cur-base p))
                        (sexpr0 (read-sexpr-from-file full))
                        (sexpr-expanded (expand1 sexpr0 (dirname full)))
                        (names (collect-node-names sexpr-expanded))
                        (rmap (make-rename-map names prefix)))
                   (rename-with-map sexpr-expanded rmap)))
                (('components . items)
                 (let ((expanded-items
                        (map (lambda (it)
                               (cond
                                ((string? it)
                                 (expand1 `(component ,it) cur-base))
                                ((symbol? it)
                                 (expand1 `(component ,it) cur-base))
                                ((and (pair? it)
                                      (or (string? (car it)) (symbol? (car it)))
                                      (null? (cdr it)))
                                 (expand1 `(component ,(car it)) cur-base))
                                ((and (pair? it)
                                      (or (string? (car it)) (symbol? (car it)))
                                      (pair? (cdr it)))
                                 (expand1 `(component ,(car it) ,(cadr it)) cur-base))
                                (else
                                 (error "components: each item must be \"path\" or (\"path\" prefix); symbols also allowed" it))))
                             items)))
                   `(par ,@expanded-items)))
                (('use-components . _)
                 (error "use-components: deprecated; use (components ...) instead"))
                (('par . es)
                 (let ((es2 (map (lambda (e) (expand1 e cur-base)) es)))
                   `(par ,@es2)))
                ((? pair?)
                 (cons (expand1 (car x) cur-base) (expand1 (cdr x) cur-base)))
                (else x)))))
    (expand1 sexpr base-dir)))


(define (parse-icnu-string-to-net icnu-string . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr0 (read-sexpr-from-string icnu-string))
         (sexpr (expand-components sexpr0 "."))
         (use-nu? (opt-ref opts 'use-nu? #t)))
    (parse-net sexpr use-nu?)))

(define *default-reduction-passes*
  (lambda ()
    (list rewrite-pass-const-fold!
          rewrite-pass-if-fold!
          rewrite-pass-AA-merge!
          rewrite-pass-AC!
          rewrite-pass-AE!
          rewrite-pass-CE-annihilation!
          rewrite-pass-wire-cleanup!)))

(define (ic-only-reduction-passes)
  (list rewrite-pass-AA-merge!
        rewrite-pass-AC!
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
                     (let ((proc (eval v (resolve-module '(icnu rewrite)))))
                       (if (procedure? proc) (proc) (*default-reduction-passes*))))
                    ((and (list? v) (list? (car v))
                          (icnu-andmap (lambda (x) (or (procedure? x) (symbol? x))) (car v)))
                     (map (lambda (p) (if (procedure? p) p (eval p (resolve-module '(icnu rewrite))))) (car v)))
                    ((list? v)
                     (map (lambda (p) (if (procedure? p) p (eval p (resolve-module '(icnu rewrite))))) v))
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



(define (resolve-from-out-name net out-name out-port opts)
  (let ((primary (try-primary-port net out-name out-port)))
    (if (not (eq? primary *unresolved*))
        primary
        (let ((other (try-other-ports net out-name out-port)))
          (if (not (eq? other *unresolved*))
              other
              (final-fallback net out-name))))))

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
         (sexpr (expand-components sexpr0 "."))
         (net (parse-net sexpr (opt-ref opts 'use-nu? #t))))
    (eval-net net opts)))

(define (parse-icnu-file-to-net path . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr0 (read-sexpr-from-file path))
         (sexpr (expand-components sexpr0 (dirname path)))
         (use-nu? (opt-ref opts 'use-nu? #t)))
    (parse-net sexpr use-nu?)))

(define (eval-icnu-file path . maybe-opts)
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (net (parse-icnu-file-to-net path opts)))
    (eval-net net opts)))

