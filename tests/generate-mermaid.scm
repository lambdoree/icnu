(use-modules (ice-9 popen))
(use-modules (icnu icnu))
(use-modules (icnu stdlib icnu-lib))
(use-modules (icnu tools icnu-proof))
(use-modules (icnu tools icnu-mermaid))
(use-modules (icnu eval))
(use-modules (icnu utils format)
             (icnu utils log))

(set-debug-level! 0)

(define out-dir "tests/mermaid")

(define (ensure-out-dir)
  (let ((cmd (string-append "mkdir -p " out-dir)))
	(system cmd)
	#t))

(define (write-net-mermaid net path)
  (write-mermaid-file net path)
  (format-string #t "wrote ~a~%" path)
  path)

(define (safe-name s)
  (let ((str (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
	(let loop ((chars (string->list str)) (acc ""))
	  (if (null? chars)
		  acc
		  (let ((c (car chars)))
			(let ((ok (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-) (char=? c #\_))))
			  (loop (cdr chars) (string-append acc (if ok (string c) "_")))))))))

(define (pad3 n)
  (let ((s (number->string n)))
    (cond ((< n 10) (string-append "00" s))
	      ((< n 100) (string-append "0" s))
          (else s))))

(define (generate-and-write name sexpr-str max-steps)
  (format-string #t "Processing example: ~a~%" name)
  (let* ((sexpr (if (string? sexpr-str) (read-sexpr-from-string sexpr-str) sexpr-str))
		 (net   (parse-net sexpr))
		 (seq   (small-step-sequence-net (copy-net net) max-steps)))
	(let loop ((ns seq) (i 0))
	  (when (pair? ns)
		(let* ((n (car ns))
			   (fname (string-append out-dir "/" (safe-name name) "-step-" (pad3 i) ".mmd")))
		  (write-net-mermaid n fname))
		(loop (cdr ns) (+ i 1))))
	(let* ((big (reduce-net-to-normal-form (copy-net net) '((max-iter . 1000))))
		   (bfname (string-append out-dir "/" (safe-name name) "-bigstep.mmd")))
	  (write-net-mermaid big bfname))
	#t))

(define examples
  (list
   (cons 'aa-merge
		 "(par (node a A) (node b A) (wire (a p) (b p)))")
   (cons 'const-fold
		 "(par (node lt1 A 'prim/lt) (node num-2 A 'lit/num 2) (node num-3 A 'lit/num 3) (node out A) (wire (num-2 p) (lt1 l)) (wire (num-3 p) (lt1 r)) (wire (lt1 p) (out p)))")
   (cons 'if-fold-true
		 "(par (node if-impl A 'prim/if) (node cond-copy C) (node lit-true A 'lit/bool #t) (node then-lit A) (node else-lit A) (wire (lit-true p) (cond-copy p)) (wire (cond-copy l) (if-impl p)) (wire (if-impl l) (then-lit p)) (wire (if-impl r) (else-lit p)))")
   (cons 'copy-fanout-small
		 "(par (node in A 'lit/num 42) (node c1 C) (node out1 A) (node out2 A) (wire (in p) (c1 p)) (wire (c1 l) (out1 p)) (wire (c1 r) (out2 p)))")
   (cons 'church-apply-3
		 (IC_CHURCH-APPLY 3 'f 'x 'outc))
   (cons 'complex-composite
		 "(par (node a A) (node c C) (wire (a p) (c p)) (node a2 A) (node e E) (wire (a2 p) (e p)) (node lt A 'prim/lt) (node num-2 A 'lit/num 2) (node num-3 A 'lit/num 3) (node out A) (wire (num-2 p) (lt l)) (wire (num-3 p) (lt r)) (wire (lt p) (out p)))")
   ))

(define (main)
  (ensure-out-dir)
  (for-each
   (lambda (pair)
	 (let ((name (car pair))
           (sexpr (cdr pair)))
       (catch #t
		 (lambda () (generate-and-write name sexpr 40) #t)
		 (lambda key
		   (format-string #t "ERROR processing ~a: ~a~%" name key)))))
   examples)
  (format-string #t "Mermaid generation complete. Files written to: ~a~%" out-dir)
  #t)

(main)
