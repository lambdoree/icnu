(define-module (icnu rewrite)
  #:use-module (icnu utils internal)
  #:use-module (icnu utils compat)
  #:use-module (icnu utils strings)
  #:use-module (ice-9 match)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu icnu)
  #:use-module ((icnu literals) #:prefix lit:)
  #:export (rewrite-pass-AC!
			rewrite-pass-AE!
			rewrite-pass-if-fold!
			rewrite-pass-const-fold!
			rewrite-pass-wire-cleanup!
			rewrite-pass-AA-merge!
			rewrite-pass-CE-annihilation!
			resolve-literal-ep
			is-literal-node?
			get-literal-value
			*unresolved*
			*resolve-literal-limit*
			))

(define *unresolved* (string->symbol "icnu-unresolved"))

(define *resolve-literal-limit* 512)

(define (rule-AA! net a-name b-name pass-through?)
  (when (and (eq? (node-agent net a-name) 'A)
			 (eq? (node-agent net b-name) 'A))
	(debugf 1 "rule-AA!: attempting on ~a and ~a\n" a-name b-name)
	(let* ((a_p (cons a-name 'p)) (a_l (cons a-name 'l)) (a_r (cons a-name 'r))
		   (b_p (cons b-name 'p)) (b_l (cons b-name 'l)) (b_r (cons b-name 'r))
		   (P_al (peer net a_l))
		   (P_ar (peer net a_r))
		   (P_bl (peer net b_l))
		   (P_br (peer net b_r))
		   (a-is-lit (is-literal-node? net a-name))
		   (b-is-lit (is-literal-node? net b-name)))
	  (cond
	   (pass-through?
		(debugf 2 "rule-AA!: skipping on ~a and ~a (pass-through: ~a)\n" a-name b-name pass-through?)
		#f)
	   ((or a-is-lit b-is-lit)
		#f)
	   (else
		(let* ((n_sym (make-fresh-name net "a"))
			   (n_p (cons n_sym 'p)) (n_l (cons n_sym 'l)) (n_r (cons n_sym 'r)))
		  (add-node! net n_sym 'A)
		  (inherit-nu! net n_sym a-name b-name)
		  (unmark-nu! net a-name)
		  (unmark-nu! net b-name)
		  (unlink-port! net a_p) (unlink-port! net a_l) (unlink-port! net a_r)
		  (unlink-port! net b_l) (unlink-port! net b_r)
		  (when (and P_al P_bl) (link-peers! net P_al P_bl))
		  (when P_ar (link-peers! net P_ar n_r))
		  (when P_br (link-peers! net P_br n_l))
		  (delete-node! net a-name)
		  (delete-node! net b-name)
		  (debugf 1 "rule-AA!: applied on ~a and ~a, created ~a\n" a-name b-name n_sym)
		  #t))))))

(define (rule-AC! net a-name c-name)
  (if (and (eq? (node-agent net a-name) 'A)
		   (eq? (node-agent net c-name) 'C))
	  (let ((c-p-peer (peer net (cons c-name 'p))))
        (when (equal? c-p-peer (cons a-name 'p))
		  (let ((c-l-peer (peer net (cons c-name 'l)))
				(c-r-peer (peer net (cons c-name 'r)))
				(a-l (cons a-name 'l))
				(a-r (cons a-name 'r)))
			(when c-l-peer (rewire! net c-l-peer a-l))
			(when c-r-peer (rewire! net c-r-peer a-r))
			(unlink-port! net (cons a-name 'p))
			(delete-node! net c-name)
			(debugf 1 "rule-AC!: applied on ~a and ~a\n" a-name c-name)
			#t)))
	  #f))

(define (rule-AE! net a-name e-name)
  (if (and (eq? (node-agent net a-name) 'A) (eq? (node-agent net e-name) 'E))
	  (let ((e-p-peer (peer net (cons e-name 'p))))
        (when (equal? e-p-peer (cons a-name 'p))
		  (delete-node! net a-name)
		  (delete-node! net e-name)
		  (debugf 1 "rule-AE!: applied on ~a and ~a\n" a-name e-name)
		  #t))
	  #f))

(define (rule-CE! net c-name e-name)
  (if (and (eq? (node-agent net c-name) 'C) (eq? (node-agent net e-name) 'E))
	  (let ((e-p-peer (peer net (cons e-name 'p))))
        (when (equal? e-p-peer (cons c-name 'p))
		  (delete-node! net c-name)
		  (delete-node! net e-name)
		  (debugf 1 "rule-CE!: applied on ~a and ~a\n" c-name e-name)
		  #t))
	  #f))

(define (is-literal-node? net node-name)
  (lit:ic-literal? net node-name))

(define (get-literal-value net node-name)
  (lit:ic-literal-value net node-name))

(define (ep-key ep)
  (if (and (pair? ep) (symbol? (car ep)) (symbol? (cdr ep)))
	  (string-append (symbol->string (car ep)) "|" (symbol->string (cdr ep)))
	  (format-string #f "~a" ep)))


(define (peers-of net n)
  (values (peer net (cons n 'p))
		  (peer net (cons n 'l))
		  (peer net (cons n 'r))))


(define (follow-port-peer net n port)
  (case port
	((p) (peer net (cons n 'p)))
	((l) (peer net (cons n 'l)))
	((r) (peer net (cons n 'r)))
	(else #f)))

(define (resolve-from-literal-node net n)
  (get-literal-value net n))

(define (resolve-from-A-node net n current-port k seen recur)
  (let* ((s (symbol->string n))
         (tag (node-tag net n)))
	(if (memq tag '(prim/eq prim/lt prim/gt prim/add prim/if))
		*unresolved*
		(call-with-values
			(lambda () (peers-of net n))
		  (lambda (p-peer l-peer r-peer)
			(let ((next (follow-port-peer net n current-port)))
			  (cond
			   (next (recur next (- k 1) seen))
			   ((and (not l-peer) (not r-peer) p-peer) ; pass-through
				(recur p-peer (- k 1) seen))
			   (else *unresolved*))))))))

(define (resolve-from-C-node net n k seen recur)
  (call-with-values
	  (lambda () (peers-of net n))
	(lambda (p-peer l-peer r-peer)
	  (let try ((lst (list p-peer l-peer r-peer)))
		(if (null? lst)
			*unresolved*
			(let ((pp (car lst)))
			  (if pp
				  (let ((res (recur pp (- k 1) seen)))
					(if (not (eq? res *unresolved*))
						res
						(try (cdr lst))))
				  (try (cdr lst)))))))))


(define (resolve-literal-ep net ep . maybe-limit)
  (let ((limit (if (null? maybe-limit) *resolve-literal-limit* (car maybe-limit))))
    (let ((queue (list (cons ep limit)))
          (seen  (make-hash-table)))
      (let loop ((q queue))
        (if (null? q)
            *unresolved*
            (let* ((item (car q))
                   (rest (cdr q))
                   (current-ep (car item))
                   (k (cdr item)))
              (if (or (not current-ep) (<= k 0))
                  (loop rest)
                  (let ((key (ep-key current-ep)))
                    (if (hash-ref seen key #f)
                        (loop rest)
                        (begin
                          (hash-set! seen key #t)
                          (if (and (pair? current-ep) (symbol? (car current-ep)))
                              (let* ((n    (car current-ep))
                                     (port (and (symbol? (cdr current-ep)) (cdr current-ep)))
                                     (agent (node-agent net n))
                                     (tag (node-tag net n)))
                                (cond
                                  ((is-literal-node? net n)
                                   (resolve-from-literal-node net n))

                                  ((and (eq? agent 'A)
                                        (not (memq tag '(prim/eq prim/lt prim/gt prim/add prim/if))))
                                   (let ((p (peer net (cons n port)))
                                         (l (peer net (cons n 'l)))
                                         (r (peer net (cons n 'r))))
                                     (let ((new-q rest))
                                       (define (enqueue ep)
                                         (when (and ep
                                                    (pair? ep)
                                                    (let ((peer-name (car ep)))
                                                      (not (and (eq? (node-agent net peer-name) 'E)
                                                                (eq? (cdr ep) 'p)))))
                                           (set! new-q (append new-q (list (cons ep (- k 1)))))))
                                       ;; prioritize p over l/r, and skip E.p
                                       (enqueue p)
                                       (enqueue l)
                                       (enqueue r)
                                       (loop new-q))))

                                  ((eq? agent 'C)
                                   (let ((p (peer net (cons n 'p)))
                                         (l (peer net (cons n 'l)))
                                         (r (peer net (cons n 'r))))
                                     (let ((adds (icnu-filter (lambda (x) x)
                                                         (list (and p (cons p (- k 1)))
                                                               (and l (cons l (- k 1)))
                                                               (and r (cons r (- k 1)))))))
                                       (loop (append rest adds)))))

                                  (else
                                   (loop rest))))
                              (loop rest))))))))))))

(define (rewrite-pass-AC! net)
  (let ((changed? #f)
		(pairs (find-active-pairs net)))
	(for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (c . 'C))
          (let ((skip? (let ((p (peer net (cons c 'p)))
                             (l (peer net (cons c 'l)))
                             (r (peer net (cons c 'r))))
						 (or (and p (eq? 'prim/if (node-tag net (car p))))
							 (and l (eq? 'prim/if (node-tag net (car l))))
							 (and r (eq? 'prim/if (node-tag net (car r))))))))
			(unless skip?
              (when (rule-AC! net a c)
				(set! changed? #t)))))
         (((c . 'C) (a . 'A))
          (let ((skip? (let ((p (peer net (cons c 'p)))
                             (l (peer net (cons c 'l)))
                             (r (peer net (cons c 'r))))
						 (or (and p (eq? 'prim/if (node-tag net (car p))))
							 (and l (eq? 'prim/if (node-tag net (car l))))
							 (and r (eq? 'prim/if (node-tag net (car r))))))))
			(unless skip?
              (when (rule-AC! net a c)
				(set! changed? #t)))))
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
			(set! changed? #t)))
         (((e . 'E) (a . 'A))
          (when (rule-AE! net a e)
			(set! changed? #t)))
         (_ #f)))
     pairs)
	changed?))

(define (ensure-global-bool-node net val)
  (let ((name (icnu-gensym "lit-bool-")))
    (add-node! net name 'A)
    (set-node-tag! net name 'lit/bool)
    (set-node-meta! net name val)
    (mark-nu! net name)
    name))

(define (ensure-global-num-node net val)
  (let ((name (icnu-gensym "lit-num-")))
    (add-node! net name 'A)
    (set-node-tag! net name 'lit/num)
    (set-node-meta! net name val)
    (mark-nu! net name)
    name))

(define (rewrite-pass-if-fold! net)
  (let ((changed? #f))
	(for-each
	 (lambda (if-name)
	   (when (eq? (node-tag net if-name) 'prim/if)
		 (let* ((p-peer (peer net (cons if-name 'p)))
				(cond-copy (and p-peer (car p-peer)))
				(cond-ep (and cond-copy (peer net (cons cond-copy 'p))))
				(cond-val (and cond-ep (resolve-literal-ep net cond-ep *resolve-literal-limit*))))
		   (when (boolean? cond-val)
			 (let* ((kept-port (if cond-val 'l 'r))
					(pruned-port (if cond-val 'r 'l))
					(kept-branch-ep (peer net (cons if-name kept-port)))
					(pruned-branch-ep (peer net (cons if-name pruned-port)))
					(output-dest (and cond-copy (peer net (cons cond-copy 'r)))))
			   (let ((kept-copier (and kept-branch-ep (car kept-branch-ep))))
				 (when (and kept-copier output-dest)
				   (let ((value-source (peer net (cons kept-copier 'p))))
					 (when value-source
					   (let ((source-node-name (car value-source)))
						 (if (is-literal-node? net source-node-name)
							 (rewire! net output-dest value-source)
							 (let ((real-source (peer net value-source)))
							   (when real-source
								 (rewire! net output-dest real-source)))))))))
			   (when pruned-branch-ep (delete-node! net (car pruned-branch-ep)))
			   (delete-node! net if-name)
			   (when cond-copy (delete-node! net cond-copy))
			   (set! changed? #t))))))
	 (all-nodes-with-agent net 'A))
	changed?))

(define (rewrite-pass-const-fold! net)
  (let ((changed? #f))
	(for-each
	 (lambda (n)
	   (let ((tag (node-tag net n)))
		 (when (memq tag '(prim/eq prim/lt prim/gt prim/add))
		   (let* ((l-ep (peer net (cons n 'l)))
				  (r-ep (peer net (cons n 'r)))
				  (l-val (if l-ep (resolve-literal-ep net l-ep *resolve-literal-limit*) *unresolved*))
				  (r-val (if r-ep (resolve-literal-ep net r-ep *resolve-literal-limit*) *unresolved*)))
			 (when (and (not (eq? l-val *unresolved*)) (not (eq? r-val *unresolved*)))
			   (let ((res
					  (case tag
						((prim/lt) (and (number? l-val) (number? r-val) (< l-val r-val)))
						((prim/gt) (and (number? l-val) (number? r-val) (> l-val r-val)))
						((prim/eq) (equal? l-val r-val))
						((prim/add) (and (number? l-val) (number? r-val) (+ l-val r-val)))
						(else #f))))
				 (when (or (boolean? res) (number? res))
				   (let ((lit (if (boolean? res)
								  (ensure-global-bool-node net res)
								  (ensure-global-num-node net res)))
						 (out-ep (peer net (cons n 'p))))
					 (when out-ep (rewire! net out-ep (cons lit 'p)))
					 (delete-node! net n)
					 (set! changed? #t)
					 (debugf 1 "rewrite-pass-const-fold!: folded ~a -> ~a\n" n res)))))))))
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
		  (let ((ta (node-tag net a)) (tb (node-tag net b)))
			(unless (or (eq? ta 'user/output) (eq? tb 'user/output))
			  (when (rule-AA! net a b #f)
				(set! changed? #t)))))
		 (_ #f)))
	 pairs)
	changed?))

(define (rewrite-pass-CE-annihilation! net)
  (let ((changed? #f)
		(pairs (find-active-pairs net)))
	(for-each
	 (lambda (pair)
	   (match pair
		 (((c . 'C) (e . 'E))
		  (when (rule-CE! net c e) (set! changed? #t)))
		 (((e . 'E) (c . 'C))
		  (when (rule-CE! net c e) (set! changed? #t)))
		 (_ #f)))
	 pairs)
	changed?))

