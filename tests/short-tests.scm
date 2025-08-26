(use-modules (icnu utils format)
             (icnu utils assertions)
             (srfi srfi-1)
             (icnu icnu)
             (icnu rewrite)
             (icnu eval))

(define (test-resolve-literal-deep-chain)
  "Deep copier chain: resolve-literal-ep should traverse many Copier nodes and find the literal."
  (let* ((depth 128)
         (nodes '())
         (wires '()))
    ;; source literal and final out node
    (set! nodes (cons (mk-node 'num-1 'A) nodes))
    (set! nodes (cons (mk-node 'out 'A) nodes))
    ;; build copier chain names c1..cN and wires:
    (let loop ((i 1))
      (when (<= i depth)
        (let ((cname (string->symbol (format-string #f "c~a" i))))
          (set! nodes (cons (mk-node cname 'C) nodes))
          (if (= i 1)
              (set! wires (cons (mk-wire 'num-1 'p cname 'p) wires))
              (let ((prev (string->symbol (format-string #f "c~a" (- i 1)))))
                (set! wires (cons (mk-wire prev 'l cname 'p) wires)))))
        (loop (+ i 1))))
    ;; wire last copier to out
    (set! wires (cons (mk-wire (string->symbol (format-string #f "c~a" depth)) 'l 'out 'p) wires))
    (let ((sexpr (apply mk-par (append nodes wires))))
      (let ((net (parse-net sexpr)))
        (assert-eq (resolve-literal-ep net (endpoint 'out 'p)) 1 "deep copier chain resolves to literal 1")))
    #t))

(define (test-resolve-literal-cycle_detection)
  "A longer cycle: ensure resolve-literal-ep does not loop indefinitely and returns *unresolved*."
  (let* ((sexpr (mk-par
                 (mk-node 'c1 'C) (mk-node 'c2 'C) (mk-node 'out 'A)
                 (mk-wire 'c1 'l 'c2 'p)
                 (mk-wire 'c2 'l 'c1 'p)
                 (mk-wire 'out 'p 'c1 'p))))
    (let ((net (parse-net sexpr)))
      (assert-eq (resolve-literal-ep net (endpoint 'out 'p)) *unresolved* "cycle resolves to *unresolved*")))
  #t)

(define (test-reduce-unlimited-equivalence)
  "Compare reduce-net-to-normal-form with a large numeric max-iter vs (max-iter . #f) (unlimited) for a chain of AA merges."
  (let* ((n 16)
         (nodes '())
         (wires '()))
    (let loop ((i 1))
      (if (> i n)
          #t
          (begin
            (set! nodes (cons (mk-node (string->symbol (format-string #f "a~a" i)) 'A) nodes))
            (when (> i 1)
	      (set! wires (cons (mk-wire (string->symbol (format-string #f "a~a" (- i 1))) 'p
					 (string->symbol (format-string #f "a~a" i)) 'p)
                                wires)))
            (loop (+ i 1)))))
    (let ((sexpr (apply mk-par (append nodes wires)))
          (opts-large '((max-iter . 100)))
          (opts-unlim '((max-iter . #f))))
      (let ((net1 (parse-net sexpr))
            (net2 (parse-net sexpr)))
        (let ((r1 (reduce-net-to-normal-form net1 opts-large))
	      (r2 (reduce-net-to-normal-form net2 opts-unlim)))
          (assert-eq (format-string #f "~a" (pretty-print r1 '((show-nu? . #t))))
                     (format-string #f "~a" (pretty-print r2 '((show-nu? . #t))))
                     "unlimited reduction equals large numeric cap"))))
    #t))

(define (run-all-short-tests)
  (let ((tests (list
                test-resolve-literal-deep-chain
                test-resolve-literal-cycle_detection
                test-reduce-unlimited-equivalence)))
    (display "Running short tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (catch 'short-tests (lambda () (t) #t) (lambda args (display "EXN\n") #f))))
                  (if res (display "ok\n") (display "FAIL\n"))))
	      tests)
    (display "All short tests completed.\n")
    #t))

(run-all-short-tests)
