(define-module (icnu tools icnu-proof)
  #:use-module (ice-9 match)
  #:use-module (icnu utils internal)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu eval)
  #:use-module (icnu utils format)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils compat)
  #:export (small-step-string big-step-string
                              small-step-net big-step-net
                              small-step-sequence-string small-step-sequence-net
                              run-steps-on-string))




(define *last-rule-applied* (icnu-make-parameter #f))

(define (apply-one-local-rule! net)
  (cond
   ((rewrite-pass-const-fold! net)
    (*last-rule-applied* 'const-fold) #t)
   ((rewrite-pass-if-fold! net)
    (*last-rule-applied* 'if-fold) #t)
   ((rewrite-pass-AA-merge! net)
    (*last-rule-applied* 'AA-merge) #t)
   ((rewrite-pass-AC! net)
    (*last-rule-applied* 'AC) #t)
   ((rewrite-pass-AE! net)
    (*last-rule-applied* 'AE) #t)
   ((rewrite-pass-CE-annihilation! net)
    (*last-rule-applied* 'CE-annihilation) #t)
   ((rewrite-pass-wire-cleanup! net)
    (*last-rule-applied* 'wire-cleanup) #t)
   (else
    (*last-rule-applied* #f)
    #f)))


(define (small-step-net net)
  (let ((copy (copy-net net)))
    (if (apply-one-local-rule! copy)
        copy
        #f)))

(define (big-step-net net . maybe-opts)
  (let ((opts (if (null? maybe-opts) '() (car maybe-opts))))
    (reduce-net-to-normal-form net opts)))

(define (small-step-string s)
  (let* ((net (parse-icnu-string-to-net s))
         (next (small-step-net net)))
    (if next
        (format-string #f "~a" (pretty-print next '((show-nu? . #t))))
        #f)))

(define (big-step-string s)
  (let* ((net (parse-icnu-string-to-net s))
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
  (let* ((net (parse-icnu-string-to-net s))
         (nets (small-step-sequence-net net max-steps)))
    (map (lambda (n) (format-string #f "~a" (pretty-print n '((show-nu? . #t))))) nets)))

(define (run-steps-on-string s . maybe-max)
  (let* ((max (if (null? maybe-max) 100 (car maybe-max)))
         (start-net (parse-icnu-string-to-net s)))
    ;; use string-join-list from icnu.utils.strings

    (define (summarize-net net)
      (let ((cntA 0) (cntC 0) (cntE 0) (a-names '()) (lits '()))
        (hash-for-each
         (lambda (name agent)
           (cond
            ((eq? agent 'A) (set! cntA (+ cntA 1)) (set! a-names (cons name a-names)))
            ((eq? agent 'C) (set! cntC (+ cntC 1)))
            ((eq? agent 'E) (set! cntE (+ cntE 1))))
           (when (is-literal-node? net name)
             (set! lits (cons (format-string #f "~a" (get-literal-value net name)) lits))))
         (net-nodes net))
        (let* ((a-short (let ((lst (reverse a-names)))
                          (let ((take (lambda (n l) (if (<= (length l) n) l (let loop ((i 0) (xs l) (acc '()))
                                                                         (if (or (null? xs) (>= i n)) (reverse acc)
                                                                             (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))))
                            (map symbol->string (take 6 lst)))))
               (a-short-str (if (null? a-short) "" (string-join-list a-short ",")))
               (lits-str (string-join-list (reverse lits) ",")))
          (format-string #f "A:~a C:~a E:~a A-names:~a Lits:~a"
                         cntA cntC cntE a-short-str lits-str))))

    (let loop ((i 0) (cur start-net) (prev-str #f))
      (let ((cur-str (summarize-net cur)))
        (format-string #t "---- step ~a ----~%" i)
        (format-string #t "~a~%" cur-str)
        (if (>= i max)
            #t
            (let ((next (small-step-net cur)))
              (if (not next)
                  #t
                  (let* ((rule (*last-rule-applied*))
                         (next-str (summarize-net next)))
                    (format-string #t "Applied: ~a~%" (if rule rule 'none))
                    (format-string #t "=> ~a~%" next-str)
                    (if (or (equal? cur-str next-str) (and prev-str (equal? prev-str cur-str)))
                        #t
                        (loop (+ i 1) next cur-str)))))))))
  #t)


