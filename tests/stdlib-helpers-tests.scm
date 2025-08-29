(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu stdlib icnu-lib)
             (icnu utils log)
             (tests test-runner))

(set-debug-level! 0)


(define (test-normalize-ep-symbol)
  (let ((res (normalize-ep 'my-node 'p)))
    (assert-eq (car res) 'my-node "normalize-ep returns correct node name")
    (assert-eq (cadr res) 'p "normalize-ep returns default port when given")
    #t))

(define (test-normalize-ep-list)
  (let ((res (normalize-ep '(other-node l) 'p)))
    (assert-eq (car res) 'other-node "normalize-ep preserves node name from list")
    (assert-eq (cadr res) 'l "normalize-ep preserves port from list")
    #t))

(define (test-ensure-number-valid)
  (ensure-number 42 "test")
  (assert-true #t "ensure-number accepts a valid number")
  #t)

(define (test-ensure-number-invalid)
  (let ((caught #f))
    (catch #t
      (lambda () (ensure-number 'not-a-number "test") #f)
      (lambda (key . args) (set! caught #t)))
    (assert-true caught "ensure-number raises error on non-number")
    #t))

(define (test-gensyms)
  (let ((syms (gensyms "tmp-" 5)))
    (let ((seen (make-hash-table))
          (unique? #t))
      (for-each (lambda (s)
                  (if (hash-ref seen s #f)
                      (set! unique? #f)
                      (hash-set! seen s #t)))
                syms)
      (assert-true (and (= (length syms) 5) unique?)
                   "gensyms creates correct number of unique symbols"))
    #t))

(run-tests "StdLib-Helpers"
           (list
            test-normalize-ep-symbol
            test-normalize-ep-list
            test-ensure-number-valid
            test-ensure-number-invalid
            test-gensyms))

