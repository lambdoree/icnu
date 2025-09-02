(define-module (icnu eval)
  #:use-module (icnu utils internal)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu tools icnu-validate)
  #:export (eval-icnu-string eval-net reduce-net-to-normal-form *default-reduction-passes* read-sexpr-from-string))

(define (read-sexpr-from-string s)
  (call-with-input-string s read))

(define *default-reduction-passes*
  (lambda ()
    (list rewrite-pass-const-fold!
          rewrite-pass-if-fold!
          rewrite-pass-AA-merge!
          rewrite-pass-AC!
          rewrite-pass-AE!
          rewrite-pass-CE-annihilation!
          rewrite-pass-wire-cleanup!)))

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
         (passes (let ((v (assq-ref opts 'passes))) (if v v (*default-reduction-passes*)))))
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
  (filter (lambda (p) (not (eq? p primary))) '(r l p)))

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


(define (resolve-from-out-name net out-name out-port)
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
      (resolve-from-out-name net out-name out-port))
     ((eq? result-form 'net)
      net)
     (else
      (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))))
  )

(define (eval-net net . maybe-opts)
  "Reduces a net to normal form and extracts a result."
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (reduced-net (reduce-net-to-normal-form net opts)))
    (extract-result-from-net reduced-net opts)))

(define (eval-icnu-string icnu-string . maybe-opts)
  "The main evaluation function. Parses a string, reduces it, and returns a result."
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr (read-sexpr-from-string icnu-string))
         (net (parse-net sexpr)))
    (eval-net net opts)))
