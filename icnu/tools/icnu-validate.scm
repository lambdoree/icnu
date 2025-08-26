(define-module (icnu tools icnu-validate)
  #:use-module (icnu icnu)
  #:use-module (srfi srfi-1)
  #:export (validate-ir))

(define (validate-ir net)
  (let ((errors '()))
    ;; Check 1: All nodes are A, C, or E (or V for free names).
    (hash-for-each
     (lambda (name agent)
       (unless (memq agent '(A C E V))
         (set! errors (cons `(invalid-agent ,name ,agent) errors))))
     (net-nodes net))

    ;; Check 2: links must be well-formed and reciprocal
    (let ((links (net-links net)))
      (hash-for-each
       (lambda (port peer)
         (unless (and (pair? port) (pair? peer))
           (set! errors (cons `(bad-link-format ,port ,peer) errors)))
         (let ((recip (hash-ref links peer #f)))
           (unless (equal? recip port)
             (set! errors (cons `(non-reciprocal-link ,port ,peer ,recip) errors)))))
       links))

    ;; Return the list of errors. An empty list indicates success.
    (reverse errors)))
