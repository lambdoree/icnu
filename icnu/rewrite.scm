(define-module (icnu rewrite)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu icnu)
  #:export (rewrite-pass-copy-fold!
            rewrite-pass-AE!
            rewrite-pass-if-fold!
            rewrite-pass-const-fold!
            rewrite-pass-wire-cleanup!
            rewrite-pass-AA-merge!
            resolve-literal-ep
            is-literal-node?
            get-literal-value))

;; Sentinel used by resolvers to indicate an unresolved / uncomputable result.
;; Use a stable symbol instead of #f to avoid conflation with boolean false.
(define *unresolved* (string->symbol "icnu-unresolved"))

(define (rule-AA! net a-name b-name pass-through?)
  (when (and (eq? (node-agent net a-name) 'A)
             (eq? (node-agent net b-name) 'A))
    (debugf 1 "rule-AA!: attempting on ~a and ~a\n" a-name b-name)
    (let* ((a_p (cons a-name 'p)) (a_l (cons a-name 'l)) (a_r (cons a-name 'r))
           (b_p (cons b-name 'p)) (b_l (cons b-name 'l)) (b_r (cons b-name 'r))
           (X (peer net a_r)) (F (peer net b_l)) (Y (peer net b_r)))
      ;; Only skip when explicitly marked as pass-through; allow merging
      ;; even when auxiliary ports are absent so simple connected A-A pairs
      ;; (a.p <-> b.p) are reduced.
      (if pass-through?
          (begin
            (debugf 2 "rule-AA!: skipping on ~a and ~a (pass-through: ~a)\n" a-name b-name pass-through?)
            #f)
          (let* ((n_sym (make-fresh-name net "a"))
                 (n_p (cons n_sym 'p)) (n_l (cons n_sym 'l)) (n_r (cons n_sym 'r)))
            (add-node! net n_sym 'A)
            (mark-nu! net n_sym)
            (if F (rewire! net a_l F) (unlink-port! net a_l))
            (when Y (rewire! net n_l Y))
            (when X (rewire! net n_r X))
            (rewire! net a_p n_p)
            (delete-node! net a-name)
            (delete-node! net b-name)
            (debugf 1 "rule-AA!: applied on ~a and ~a, created ~a\n" a-name b-name n_sym)
            #t)))))

(define (rule-AC! net a-name c-name)
  (if (and (eq? (node-agent net a-name) 'A)
           (eq? (node-agent net c-name) 'C))
      (let ((c-p-peer (peer net (cons c-name 'p))))
	(when (equal? c-p-peer (cons a-name 'p))
          (let ((c-l-peer (peer net (cons c-name 'l)))
		(c-r-peer (peer net (cons c-name 'r)))
		(a-l (cons a-name 'l))
		(a-r (cons a-name 'r)))
	    (when c-l-peer (rewire! net a-l c-l-peer))
	    (when c-r-peer (rewire! net a-r c-r-peer))
	    (unlink-port! net (cons a-name 'p))
	    (delete-node! net c-name)
	    #t)))
      #f))

(define (rule-AE! net a-name e-name)
  (if (and (eq? (node-agent net a-name) 'A) (eq? (node-agent net e-name) 'E))
      (let ((e-p-peer (peer net (cons e-name 'p))))
	(when (equal? e-p-peer (cons a-name 'p))
          (delete-node! net a-name)
          (delete-node! net e-name)
          #t))
      #f))

(define (rule-CE! net c-name e-name)
  (if (and (eq? (node-agent net c-name) 'C) (eq? (node-agent net e-name) 'E))
      (let ((e-p-peer (peer net (cons e-name 'p))))
	(when (equal? e-p-peer (cons c-name 'p))
          (delete-node! net c-name)
          (delete-node! net e-name)
          #t))
      #f))

(define (is-literal-node? net node-name)
  "Return #t for nodes that should be treated as literal tokens for folding:
   - canonical literal names (lit-*, num-, str-, trig-*)
   - prefixed true/false tokens (true*, false*)
   - condition literals prefixed with \"cond-\" are also treated as literals.
   - A-nodes that have no auxiliary peers (often generated as literal tokens)
     but exclude obvious operator nodes (eq-/lt-/gt-/if-impl) to avoid misclassifying
     comparator/if implementation nodes."
  (and (symbol? node-name)
       (let ((s (symbol->string node-name)))
         (or (string-prefix? "lit-true" s)
             (string-prefix? "lit-false" s)
             (string-prefix? "num-" s)
             (string-prefix? "str-" s)
             (string-prefix? "trig-num-" s)
             (string-prefix? "trig-str-" s)
             (string-prefix? "true" s)
             (string-prefix? "false" s)
             (string-prefix? "cond-" s)
             ;; A node with no aux peers is often a token-like literal.
             (and (eq? (node-agent net node-name) 'A)
                  (not (peer net (cons node-name 'l)))
                  (not (peer net (cons node-name 'r)))
                  (not (string-prefix? "eq-" s))
                  (not (string-prefix? "lt-" s))
                  (not (string-prefix? "gt-" s))
                  (not (string-prefix? "if-impl" s)))))))

(define (get-literal-value node-name)
  "Extract a Scheme value from a literal-style node name when possible.
   Returns boolean/number/string/symbol, or #f when not recognized."
  (let ((s (symbol->string node-name)))
    (cond
     ;; boolean families
     ((or (string-prefix? "lit-true" s) (string-prefix? "true" s))  #t)
     ((or (string-prefix? "lit-false" s) (string-prefix? "false" s)) #f)
     ;; numeric families
     ((string-prefix? "num-" s) (string->number (substring s 4)))
     ((string-prefix? "trig-num-" s)
      (let* ((parts (string-split-char (substring s 9) #\-))
             (val-str (string-join-list (reverse (cdr (reverse parts))) "-")))
        (string->number val-str)))
     ;; string families
     ((string-prefix? "str-" s) (string-join-list (string-split-char (substring s 4) #\_) " "))
     ((string-prefix? "trig-str-" s)
      (let* ((parts (string-split-char (substring s 9) #\-))
             (val-str (string-join-list (reverse (cdr (reverse parts))) "-")))
        (string-join-list (string-split-char val-str #\_) " ")))
     ;; condition literals prefixed with \"cond-\" are treated as true.
     ((string-prefix? "cond-" s) #t)
     ;; Fallback
     (else (string->symbol s)))))

;; resolve-literal-ep:
;; Follow an endpoint for up to `limit` steps to find a literal value.
;; Accepts either an endpoint (returned by peer) or a node/pair endpoint.
;; Heuristics:
;;  - If endpoint is (node . p) and node is a literal node -> return its value.
;;  - If endpoint points to a C (copy) node, follow its .p peer.
;;  - If endpoint points to an A node that has no aux peers (pass-through), follow its .p peer.
;;  - If endpoint is on an aux port (l/r), prefer to resolve via copier semantics:
;;    if the aux-side node is a C (copy) gadget, follow its .p peer (the real source).
;;    otherwise follow the aux link to the connected endpoint.
;; - Track visited endpoints (stringified) to avoid oscillation between copy/aux links.
;; Returns *unresolved* when no literal found within `limit` steps.
(define (resolve-literal-ep net ep . maybe-limit)
  (let ((limit (if (null? maybe-limit) 8 (car maybe-limit))))
    (let loop ((current-ep ep) (k limit) (seen (make-hash-table)))
      (cond
       ((or (not current-ep) (zero? k)) *unresolved*)
       (else
        (let ((key (format-string #f "~a" current-ep)))
          (if (hash-ref seen key #f)
              *unresolved*
              (begin
                (hash-set! seen key #t)
                (if (and (pair? current-ep) (symbol? (car current-ep)))
                    (let* ((n (car current-ep))
                           (agent (node-agent net n)))
                      (cond
                       ((is-literal-node? net n) (get-literal-value n))
                       ((eq? agent 'A)
                        (let ((s (symbol->string n)))
                          (if (or (string-contains? s "eq")
                                  (string-contains? s "lt")
                                  (string-contains? s "gt")
                                  (string-prefix? "if-impl" s))
                              *unresolved*
                              (let* ((r-peer (peer net (cons n 'r)))
                                     (l-peer (peer net (cons n 'l)))
                                     (r-agent (and r-peer (node-agent net (car r-peer))))
                                     (l-agent (and l-peer (node-agent net (car l-peer)))))
                                (cond
                                 ((eq? r-agent 'E) #t)
                                 ((eq? l-agent 'E) #f)
                                 ((and (not l-peer) (not r-peer))
                                  (let ((p-peer (peer net (cons n 'p))))
                                    (if p-peer (loop p-peer (- k 1) seen) *unresolved*)))
                                 (r-peer (loop r-peer (- k 1) seen))
                                 (else *unresolved*))))))
                       ((eq? agent 'C)
                        (let ((p-peer (peer net (cons n 'p))))
                          (if p-peer (loop p-peer (- k 1) seen) *unresolved*)))
                       (else *unresolved*)))
                    *unresolved*)))))))))


(define (rewrite-pass-copy-fold! net)
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (c . 'C))
          (let ((skip? (let ((p (peer net (cons c 'p)))
                             (l (peer net (cons c 'l)))
                             (r (peer net (cons c 'r))))
			 (or (and p (string-prefix? "if-impl" (symbol->string (car p))))
                             (and l (string-prefix? "if-impl" (symbol->string (car l))))
                             (and r (string-prefix? "if-impl" (symbol->string (car r))))))))
		(unless skip?
		  (when (rule-AC! net a c)
                    (debugf 1 "rule-AC!: applied on ~a and ~a\n" a c)
                    (set! changed? #t)))))
          (((c . 'C) (a . 'A))
           (when (rule-AC! net a c)
             (debugf 1 "rule-AC!: applied on ~a and ~a\n" a c)
             (set! changed? #t)))
          (((a . 'A) (e . 'E))
           (when (rule-AE! net a e)
             (debugf 1 "rule-AE!: applied on ~a and ~a\n" a e)
             (set! changed? #t)))
          (((e . 'E) (a . 'A))
           (when (rule-AE! net a e)
             (debugf 1 "rule-AE!: applied on ~a and ~a\n" a e)
             (set! changed? #t)))
          (((c . 'C) (e . 'E))
           (when (rule-CE! net c e)
             (debugf 1 "rule-CE!: applied on ~a and ~a\n" c e)
             (set! changed? #t)))
          (((e . 'E) (c . 'C))
           (when (rule-CE! net c e)
             (debugf 1 "rule-CE!: applied on ~a and ~a\n" c e)
             (set! changed? #t)))
          (_ #f)))
       pairs)
     changed?))

(define (rewrite-pass-AE! net)
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (e . 'E))
          (when (rule-AE! net a e)
            (debugf 1 "rule-AE!: applied on ~a and ~a\n" a e)
            (set! changed? #t)))
         (((e . 'E) (a . 'A))
          (when (rule-AE! net a e)
            (debugf 1 "rule-AE!: applied on ~a and ~a\n" a e)
            (set! changed? #t)))
         (_ #f)))
     pairs)
    changed?))

(define (ensure-global-bool-node net val)
  (let ((name (if val (gensym "lit-true-") (gensym "lit-false-"))))
    (add-node! net name 'A)
    (mark-nu! net name)
    name))

(define (rewrite-pass-if-fold! net)
  (let ((changed? #f))
    (for-each
     (lambda (if-name)
       (when (or (string-prefix? "if-impl-" (symbol->string if-name))
                 (string-prefix? "if-impl" (symbol->string if-name)))
         (let* ((p-peer (peer net (cons if-name 'p)))
                (cond-copy (and p-peer (car p-peer)))
                (cond-ep (and cond-copy (peer net (cons cond-copy 'p))))
                ;; attempt to resolve condition value (follow up to 8 steps)
                (cond-val (and cond-ep (resolve-literal-ep net cond-ep 8))))
           (when (boolean? cond-val)
             (let* ((kept-port (if cond-val 'l 'r))
                    (pruned-port (if cond-val 'r 'l))
                    (kept-branch-ep (peer net (cons if-name kept-port)))
                    (pruned-branch-ep (peer net (cons if-name pruned-port))))
	       (when kept-branch-ep
                 (let* ((out-ep (peer net (cons if-name 'p)))
                        (kept-copier (car kept-branch-ep))
                        (value-source (peer net (cons kept-copier 'p))))
                   (when (and out-ep value-source)
                     (rewire! net out-ep value-source)))
                 (when pruned-branch-ep (delete-node! net (car pruned-branch-ep)))
                 ;; cleanup the if node and its condition copier
                 (delete-node! net if-name)
                 (when cond-copy (delete-node! net cond-copy))
                 (set! changed? #t)))))))
     (all-nodes-with-agent net 'A))
    changed?))

(define (rewrite-pass-const-fold! net)
  (let ((changed? #f))
    (for-each
     (lambda (n)
       (let ((s (symbol->string n)))
         ;; match comparator-like names robustly: prefix/suffix or embedded markers
         (when (or (string-contains? s "eq") (string-contains? s "lt") (string-contains? s "gt"))
           (let* ((l-ep (peer net (cons n 'l)))
                  (r-ep (peer net (cons n 'r)))
                  ;; resolve values by following endpoints up to depth 8
                  ;; Use explicit conditional so that a missing aux endpoint
                  ;; yields the sentinel *unresolved* rather than #f, which
                  ;; would be ambiguous with the literal boolean #f.
                  (l-val (if l-ep (resolve-literal-ep net l-ep 8) *unresolved*))
                  (r-val (if r-ep (resolve-literal-ep net r-ep 8) *unresolved*)))
             (when (and (not (eq? l-val *unresolved*)) (not (eq? r-val *unresolved*)))
	       (let ((res
		      (cond
		       ((or (string-contains? s "lt-") (string-contains? s "-lt") (string-contains? s "lt")) (and (number? l-val) (number? r-val) (< l-val r-val)))
		       ((or (string-contains? s "gt-") (string-contains? s "-gt") (string-contains? s "gt")) (and (number? l-val) (number? r-val) (> l-val r-val)))
		       ((or (string-contains? s "eq-") (string-contains? s "-eq") (string-contains? s "eq")) (equal? l-val r-val))
		       (else #f))))
                 (when (boolean? res)
                   (let ((lit (ensure-global-bool-node net res))
                         (out-ep (peer net (cons n 'p))))
                     (when out-ep (rewire! net out-ep (cons lit 'p)))
                     (delete-node! net n)
                     (set! changed? #t)
                     (debugf 1 "rewrite-pass-const-fold!: folded ~a -> ~a\n" n (if res "#t" "#f"))))))))))
     (all-nodes-with-agent net 'A))
    changed?))

(define (rewrite-pass-wire-cleanup! net)
  (let ((changed? #f))
    (for-each
     (lambda (c)
       (let ((p (peer net (cons c 'p)))
             (l (peer net (cons c 'l)))
             (r (peer net (cons c 'r))))
         (when (and (not p) (not l) (not r))
           (delete-node! net c)
           (set! changed? #t))))
     (all-nodes-with-agent net 'C))
    changed?))

(define (rewrite-pass-AA-merge! net)
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (b . 'A))
          (when (rule-AA! net a b #f)
            (set! changed? #t)))
         (_ #f)))
     pairs)
    changed?))
