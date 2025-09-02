(define-module (icnu tools icnu-proof)
  #:use-module (ice-9 match)
  #:use-module (icnu utils internal)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu eval)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:export (small-step-string big-step-string
            small-step-net big-step-net
            small-step-sequence-string small-step-sequence-net
            show-step-sequence run-steps-on-string
            demo-examples demo-run-examples))


(define (ensure-global-lit-node net tag val prefix)
  (let ((name (make-fresh-name net prefix)))
    (add-node! net name 'A)
    (set-node-tag! net name tag)
    (set-node-meta! net name val)
    (mark-nu! net name)
    name))

(define (ensure-global-bool-node net val)
  (ensure-global-lit-node net 'lit/bool val "lit-bool-"))

(define (ensure-global-num-node net val)
  (ensure-global-lit-node net 'lit/num val "lit-num-"))


(define (ap:bool/num? x) (or (boolean? x) (number? x)))

(define (is-primitive-op-node? net node-name)
  (let ((tag (node-tag net node-name)))
    (memq tag '(prim/eq prim/lt prim/gt prim/add prim/if))))


(define (ap:annihilate-if-principal-linked! net a-sym e-sym)
  (let ((e-p-peer (peer net (cons e-sym 'p))))
    (when (and (equal? e-p-peer (cons a-sym 'p)))
      (delete-node! net a-sym)
      (delete-node! net e-sym)
      #t)))

(define (ap:ac-push-through-copier! net a-sym c-sym)
  (let ((c-p-peer (peer net (cons c-sym 'p))))
    (when (and (equal? c-p-peer (cons a-sym 'p)))
      (let* ((c-l-peer (peer net (cons c-sym 'l)))
             (c-r-peer (peer net (cons c-sym 'r)))
             (a-l      (cons a-sym 'l))
             (a-r      (cons a-sym 'r)))
        (when c-l-peer (rewire! net c-l-peer a-l))
        (when c-r-peer (rewire! net c-r-peer a-r))
        (unlink-port! net (cons a-sym 'p))
        (delete-node! net c-sym)
        #t))))

(define (ap:aa-merge! net a b)
  (let* ((a_p (cons a 'p)) (a_l (cons a 'l)) (a_r (cons a 'r))
         (b_p (cons b 'p)) (b_l (cons b 'l)) (b_r (cons b 'r))
         (X (peer net a_r)) (F (peer net b_l)) (Y (peer net b_r))
         (a-is-lit (is-literal-node? net a))
         (b-is-lit (is-literal-node? net b))
         (a-is-prim (is-primitive-op-node? net a))
         (b-is-prim (is-primitive-op-node? net b))
         (a-tag (node-tag net a))
         (b-tag (node-tag net b)))
    (unless (or a-is-lit b-is-lit a-is-prim b-is-prim (eq? a-tag 'user/output) (eq? b-tag 'user/output))
      (let ((n (make-fresh-name net "a")))
        (add-node! net n 'A)
        (inherit-nu! net n a b)
        (if F (rewire! net a_l F) (unlink-port! net a_l))
        (when Y (rewire! net (cons n 'l) Y))
        (when X (rewire! net (cons n 'r) X))
        (rewire! net a_p (cons n 'p))
        (delete-node! net a)
        (delete-node! net b)
        #t))))

(define (ap:apply-pair-rule! net pair)
  (match pair
    (((a . 'A) (e . 'E)) (ap:annihilate-if-principal-linked! net a e))
    (((e . 'E) (a . 'A)) (ap:annihilate-if-principal-linked! net a e))
    (((c . 'C) (e . 'E)) (ap:annihilate-if-principal-linked! net c e))
    (((e . 'E) (c . 'C)) (ap:annihilate-if-principal-linked! net c e))
    (((a . 'A) (c . 'C)) (ap:ac-push-through-copier! net a c))
    (((c . 'C) (a . 'A)) (ap:ac-push-through-copier! net a c))
    (((a . 'A) (b . 'A)) (ap:aa-merge! net a b))
    (_ #f)))

(define (ap:apply-first-active-pair! net)
  (let loop ((ps (find-active-pairs net)))
    (cond
      ((null? ps) #f)
      (else
       (or (ap:apply-pair-rule! net (car ps))
           (loop (cdr ps)))))))


(define (ap:const-fold-one-A! net a)
  (let ((tag (node-tag net a)))
    (when (memq tag '(prim/eq prim/lt prim/gt prim/add))
      (let* ((l-ep  (peer net (cons a 'l)))
             (r-ep  (peer net (cons a 'r)))
             (l-val (if l-ep (resolve-literal-ep net l-ep *resolve-literal-limit*) *unresolved*))
             (r-val (if r-ep (resolve-literal-ep net r-ep *resolve-literal-limit*) *unresolved*)))
        (when (and (not (eq? l-val *unresolved*))
                   (not (eq? r-val *unresolved*)))
          (let ((res (case tag
                       ((prim/lt)  (and (number? l-val) (number? r-val) (< l-val r-val)))
                       ((prim/gt)  (and (number? l-val) (number? r-val) (> l-val r-val)))
                       ((prim/eq)  (equal? l-val r-val))
                       ((prim/add) (and (number? l-val) (number? r-val) (+ l-val r-val)))
                       (else #f))))
            (when (ap:bool/num? res)
              (let* ((lit (if (boolean? res)
                              (ensure-global-bool-node net res)
                              (ensure-global-num-node  net res)))
                     (out-ep (peer net (cons a 'p))))
                (when out-ep (rewire! net out-ep (cons lit 'p)))
                (delete-node! net a)
                #t))))))))

(define (ap:const-fold! net)
  (let loop ((as (all-nodes-with-agent net 'A)))
    (and (pair? as)
         (or (ap:const-fold-one-A! net (car as))
             (loop (cdr as))))))


(define (ap:if-fold-one-A! net a)
  (when (eq? (node-tag net a) 'prim/if)
    (let* ((p-peer    (peer net (cons a 'p)))
           (cond-copy (and p-peer (car p-peer)))
           (cond-ep   (and cond-copy (peer net (cons cond-copy 'p))))
           (cond-val  (and cond-ep (resolve-literal-ep net cond-ep *resolve-literal-limit*))))
      (when (boolean? cond-val)
        (let* ((kept-port       (if cond-val 'l 'r))
               (pruned-port     (if cond-val 'r 'l))
               (kept-branch-ep  (peer net (cons a kept-port)))
               (pruned-branch-ep (peer net (cons a pruned-port)))
               (output-dest     (and cond-copy (peer net (cons cond-copy 'r))))
               (kept-copier     (and kept-branch-ep (car kept-branch-ep))))
          (when (and kept-copier output-dest)
            (let ((value-source (peer net (cons kept-copier 'p))))
              (when value-source
                (rewire! net output-dest value-source))))
          (when pruned-branch-ep (delete-node! net (car pruned-branch-ep)))
          (delete-node! net a)
          (when cond-copy (delete-node! net cond-copy))
          #t)))))

(define (ap:if-fold! net)
  (let loop ((as (all-nodes-with-agent net 'A)))
    (and (pair? as)
         (or (ap:if-fold-one-A! net (car as))
             (loop (cdr as))))))


(define (ap:wire-cleanup-one-C! net c)
  (let ((p (peer net (cons c 'p)))
        (l (peer net (cons c 'l)))
        (r (peer net (cons c 'r))))
    (when (and (not p) (not l) (not r))
      (delete-node! net c)
      #t)))

(define (ap:wire-cleanup! net)
  (let loop ((cs (all-nodes-with-agent net 'C)))
    (and (pair? cs)
         (or (ap:wire-cleanup-one-C! net (car cs))
             (loop (cdr cs))))))

 
(define (apply-one-local-rule! net)
  "Delegate to the canonical rewrite-pass implementations from icnu/rewrite.
   The proof helpers duplicated pass logic which could diverge subtly from the
   main rewrite passes; use the central implementations for parity and to
   ensure small-step behavior matches rewrite-based reductions used elsewhere."
  (or (rewrite-pass-const-fold! net)          ;; constant folding
      (rewrite-pass-if-fold! net)             ;; if-fold
      (rewrite-pass-AA-merge! net)            ;; AA merges
      (rewrite-pass-AC! net)                  ;; AC push-through-copier
      (rewrite-pass-AE! net)                  ;; AE annihilation
      (rewrite-pass-CE-annihilation! net)     ;; CE annihilation
      (rewrite-pass-wire-cleanup! net)))      ;; remove orphan C nodes


(define (small-step-net net)
  (let ((copy (copy-net net)))
    (if (apply-one-local-rule! copy)
        copy
        #f)))

(define (big-step-net net . maybe-opts)
  (let ((opts (if (null? maybe-opts) '() (car maybe-opts))))
    (reduce-net-to-normal-form net opts)))

(define (small-step-string s)
  (let* ((sexpr (read-sexpr-from-string s))
         (net (parse-net sexpr))
         (next (small-step-net net)))
    (if next
        (format-string #f "~a" (pretty-print next '((show-nu? . #t))))
        #f)))

(define (big-step-string s)
  (let* ((sexpr (read-sexpr-from-string s))
         (net (parse-net sexpr))
         (reduced (big-step-net net '()))
         (out (pretty-print reduced '((show-nu? . #t)))))
    (format-string #f "~a" out)))

(define (small-step-sequence-net net max-steps)
  (let loop ((cur net) (acc (list net)) (i 0))
    (if (>= i max-steps)
        (reverse acc)
        (let ((next (small-step-net cur)))
          (if next
              (loop next (cons next acc) (+ i 1))
              (reverse acc))))))

(define (small-step-sequence-string s max-steps)
  (let* ((sexpr (read-sexpr-from-string s))
         (net (parse-net sexpr))
         (nets (small-step-sequence-net net max-steps)))
    (map (lambda (n) (format-string #f "~a" (pretty-print n '((show-nu? . #t))))) nets)))

(define (run-steps-on-string s . maybe-max)
  "Run small-step sequence for input string `s` and print a compact summary for each step.
   Returns #t on completion. Optional maybe-max (number) limits steps.

This variant prints a concise summary (agent counts, a few A node names, and literal values)
instead of the full pretty-printed net to make outputs easier to read for large nets."
  (let* ((max (if (null? maybe-max) 100 (car maybe-max)))
         (sexpr (read-sexpr-from-string s))
         (start-net (parse-net sexpr)))
    (define (join-strings lst)
      (let loop ((xs lst) (acc ""))
        (if (null? xs) acc
            (let ((s (car xs)))
              (loop (cdr xs) (if (string=? acc "") s (string-append acc "," s)))))))

    (define (summarize-net net)
      (let ((cntA 0) (cntC 0) (cntE 0) (cntV 0) (a-names '()) (lits '()))
        (hash-for-each
         (lambda (name agent)
           (cond
             ((eq? agent 'A) (set! cntA (+ cntA 1)) (set! a-names (cons name a-names)))
             ((eq? agent 'C) (set! cntC (+ cntC 1)))
             ((eq? agent 'E) (set! cntE (+ cntE 1)))
             ((eq? agent 'V) (set! cntV (+ cntV 1))))
           (when (is-literal-node? net name)
             (set! lits (cons (format-string #f "~a" (get-literal-value net name)) lits))))
         (net-nodes net))
        (let* ((a-short (let ((lst (reverse a-names)))
                          (let ((take (lambda (n l) (if (<= (length l) n) l (let loop ((i 0) (xs l) (acc '()))
                                                             (if (or (null? xs) (>= i n)) (reverse acc)
                                                                 (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))))
                            (map symbol->string (take 6 lst)))))
               (a-short-str (if (null? a-short) "" (join-strings a-short)))
               (lits-str (join-strings (reverse lits))))
          (format-string #f "A:~a C:~a E:~a V:~a A-names:~a Lits:~a"
                         cntA cntC cntE cntV a-short-str lits-str))))

    (let loop ((i 0) (cur start-net) (prev-str #f))
      (let ((cur-str (summarize-net cur)))
        (format-string #t "---- step ~a ----~%" i)
        (format-string #t "~a~%" cur-str)
        (if (>= i max)
            #t
            (let ((next (small-step-net cur)))
              (if (not next)
                  #t
                  (let ((next-str (summarize-net next)))
                    (if (or (equal? cur-str next-str) (and prev-str (equal? prev-str cur-str)))
                        #t
                        (loop (+ i 1) next cur-str)))))))))
    #t)
