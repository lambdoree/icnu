(define-module (icnu tools icnu-validate)
  #:use-module (icnu icnu)
  #:use-module (srfi srfi-1)
  #:export (validate-ir))

(define (validate-ir net)
  (let ((errors '()))
    ;; Check 1: All nodes are A, C, E, or V (free names).
    (hash-for-each
     (lambda (name agent)
       (unless (memq agent '(A C E V))
         (set! errors (cons `(invalid-agent ,name ,agent) errors))))
     (net-nodes net))

    ;; Check 2: links must be well-formed, reciprocal, and have valid ports.
    (let ((links (net-links net)))
      (hash-for-each
       (lambda (port peer)
         (if (and (pair? port) (pair? peer))
             (begin
               ;; Port symbols must be one of p, l, r.
               (unless (valid-port? (cdr port))
                 (set! errors (cons `(invalid-port ,port) errors)))
               (unless (valid-port? (cdr peer))
                 (set! errors (cons `(invalid-port ,peer) errors)))
               ;; Detect self‑loop links (a.p ↔ a.p).
               (when (and (eq? (car port) (car peer))
                          (eq? (cdr port) (cdr peer)))
                 (set! errors (cons `(self-loop ,port) errors)))
               (let ((recip (hash-ref links peer #f)))
                 (unless (equal? recip port)
                   (set! errors (cons `(non-reciprocal-link ,port ,peer ,recip) errors)))))
             (set! errors (cons `(bad-link-format ,port ,peer) errors))))
       links)

    ;; Check 3: ν‑binder names must correspond to existing nodes.
    (hash-for-each
     (lambda (nu-name _)
       (unless (hash-ref (net-nodes net) nu-name #f)
         (set! errors (cons `(nu-missing-node ,nu-name) errors))))
     (net-nu-set net))

    ;; Return the list of errors. An empty list indicates success.
    (reverse errors))))
