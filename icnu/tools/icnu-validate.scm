(define-module (icnu tools icnu-validate)
  #:use-module (icnu icnu)
  #:use-module (icnu utils internal)
  #:export (validate-ir))



(define (vi:valid-agent? ag)
  (memq ag '(A C E V)))

(define (vi:agent-allows-port? agent port)
  (if (eq? agent 'V)
      #t
      (let ((ports (get-ports agent)))
        (and ports (memq port ports)))))

(define (vi:check-agents nodes)
  (let ((errs '()))
    (hash-for-each
     (lambda (name agent)
       (unless (vi:valid-agent? agent)
         (set! errs (cons `(invalid-agent ,name ,agent) errs))))
     nodes)
    (reverse errs)))

(define (vi:check-one-link net links port peer)
  (let ((errs '()))
    (cond
     ((not (and (pair? port) (pair? peer)))
      (set! errs (cons `(bad-link-format ,port ,peer) errs)))

     (else
      (unless (valid-port? (cdr port))
        (set! errs (cons `(invalid-port ,port) errs)))
      (unless (valid-port? (cdr peer))
        (set! errs (cons `(invalid-port ,peer) errs)))

      (let* ((a-name (car port))
             (a-port (cdr port))
             (b-name (car peer))
             (b-port (cdr peer))
             (a-agent (and (symbol? a-name) (node-agent net a-name)))
             (b-agent (and (symbol? b-name) (node-agent net b-name))))
        (when (and a-agent (not (vi:agent-allows-port? a-agent a-port)))
          (set! errs (cons `(invalid-port-for-agent ,port ,a-agent) errs)))
        (when (and b-agent (not (vi:agent-allows-port? b-agent b-port)))
          (set! errs (cons `(invalid-port-for-agent ,peer ,b-agent) errs))))

      (when (and (eq? (car port) (car peer))
                 (eq? (cdr port) (cdr peer)))
        (set! errs (cons `(self-loop ,port) errs)))

      (let ((recip (hash-ref links peer #f)))
        (unless (equal? recip port)
          (set! errs (cons `(non-reciprocal-link ,port ,peer ,recip) errs))))))
    (reverse errs)))

(define (vi:check-links net)
  (let* ((links (net-links net))
         (errs  '()))
    (hash-for-each
     (lambda (port peer)
       (set! errs (append errs (vi:check-one-link net links port peer))))
     links)
    errs))

(define (vi:check-nu-binders net)
  (let* ((nodes (net-nodes net))
         (nu    (net-nu-set net))
         (errs  '()))
    (hash-for-each
     (lambda (nu-name _)
       (unless (hash-ref nodes nu-name #f)
         (set! errs (cons `(nu-missing-node ,nu-name) errs))))
     nu)
    (reverse errs)))

(define (validate-ir net)
  (let* ((nodes (net-nodes net))
         (errs1 (vi:check-agents nodes))
         (errs2 (vi:check-links net))
         (errs3 (vi:check-nu-binders net)))
    (append errs1 errs2 errs3)))
