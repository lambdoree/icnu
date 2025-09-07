(define-module (icnu icnu)
  #:use-module (icnu ic)
  #:use-module (icnu nu)
  #:use-module (icnu utils log)
  #:export (parse-net pretty-print delete-node!)
  #:re-export (mk-node mk-wire mk-par mk-node-labeled
                       empty-net copy-net make-fresh-name all-names node-agent endpoint valid-port?
                       peer net-nodes net-links net-tags net-meta get-ports unlink-port!
                       rewire! all-nodes-with-agent find-active-pairs
                       set-link-conflict-mode!
                       *link-conflict-mode*
                       mark-temporary! unmark-temporary! temporary-endpoint?
                       link-peers!
                       add-node! set-node-tag! node-tag set-node-meta! node-meta
                       <net> net?
                       mk-nu
                       mark-nu! inherit-nu! unmark-nu! node-nu?
                       ))

(define (pretty-print net . maybe-opts)
  (let* ((body (apply ic-pretty-print net maybe-opts))
         (opts (if (null? maybe-opts) '() (car maybe-opts)))
         (showNu (pp-bool opts 'show-nu? #f)))
    (if showNu
        (let ((nu-names '()))
          (hash-for-each
           (lambda (nm ag)
             (when (node-nu? net nm)
               (set! nu-names (cons nm nu-names))))
           (net-nodes net))
          (if (null? nu-names)
              body
              `(nu ,(reverse nu-names) ,body)))
        body)))

(define (delete-node! n x)
  (if (node-nu? n x)
      (let ((agent (node-agent n x)))
        (when agent
          (for-each (lambda (pt) (unlink-port! n (cons x pt))) (get-ports agent))
          (hash-set! (net-nodes n) x 'V)
          (hash-set! (net-tags n) x 'user/opaque))
        n)
      (ic-delete-node! n x)))

(define (parse-net sexpr . maybe-use-nu)
  (let ((use-nu? (if (null? maybe-use-nu) #t (car maybe-use-nu))))
    (if use-nu?
        (apply parse-net/nu (cons sexpr maybe-use-nu))
        (ic-parse-net sexpr))))
