(use-modules (icnu utils format)
             (icnu utils assertions)
             (icnu utils strings)
             (icnu utils log)
             (icnu utils helpers)
             (icnu icnu))

(define (test-string-contains)
  (assert-true (string-contains? "hello world" "hello") "string-contains? prefix")
  (assert-true (string-contains? "hello world" "world") "string-contains? suffix")
  (assert-true (string-contains? "hello world" "o w") "string-contains? middle")
  (assert-false (string-contains? "hello" "world") "string-contains? not found")
  (assert-true (string-contains? "abc" "") "string-contains? empty substring")
  (assert-false (string-contains? "" "a") "string-contains? empty string")
  #t)

(define (test-string-join-list)
  (assert-eq (string-join-list '("a" "b" "c") "-") "a-b-c" "string-join-list basic")
  (assert-eq (string-join-list '("a") "-") "a" "string-join-list single item")
  (assert-eq (string-join-list '() "-") "" "string-join-list empty list")
  #t)

(define (test-string-split-char)
  (assert-eq (string-split-char "a-b-c" #\-) '("a" "b" "c") "string-split-char basic")
  (assert-eq (string-split-char "abc" #\-) '("abc") "string-split-char no delimiter")
  (assert-eq (string-split-char "-a-b" #\-) '("" "a" "b") "string-split-char leading delimiter")
  (assert-eq (string-split-char "a-b-" #\-) '("a" "b" "") "string-split-char trailing delimiter")
  (assert-eq (string-split-char "" #\-) '("") "string-split-char empty string")
  #t)

(define (test-debug-log-levels)
  (let ((original-level (debug-level?)))
    (set-debug-level! 5)
    (assert-eq (debug-level?) 5 "set-debug-level! integer")
    (set-debug-log! #t)
    (assert-eq (debug-level?) 1 "set-debug-log! #t")
    (set-debug-log! #f)
    (assert-eq (debug-level?) 0 "set-debug-log! #f")
    ;; Restore original level to avoid side effects
    (set-debug-level! original-level)
    #t))

(define (test-format-string)
  (assert-eq (format-string #f "hello ~a" "world") "hello world" "format-string ~a")
  (assert-eq (format-string #f "val=~s" 123) "val=123" "format-string ~s")
  (assert-eq (format-string #f "a ~~ b") "a ~ b" "format-string ~~")
  (assert-eq (format-string #f "line1~%line2") "line1\nline2" "format-string ~%")
  (assert-eq (format-string #f "no args") "no args" "format-string no args")
  (assert-eq (format-string #f "too few ~a ~a" "args") "too few args ~a" "format-string too few args")
  (assert-eq (format-string #f "") "" "format-string empty")
  #t)

(define (test-wire-or-list)
  (assert-eq (wire-or-list 'a 'b) '(wire (a p) (b p)) "wire-or-list with symbols")
  (assert-eq (wire-or-list '(a l) 'b) '(wire (a l) (b p)) "wire-or-list with source endpoint")
  #t)

(define (test-log-output-functions)
  (let ((original-level (debug-level?)))
    (set-debug-level! 2)
    ;; Test warnf (level 1)
    (let ((output (call-with-output-string
                   (lambda (p)
                     (parameterize ((current-output-port p))
                       (warnf "warn message ~a" 1))))))
      (assert-eq output "warn message 1" "warnf output"))
    ;; Test debugf-limited
    (let ((output (call-with-output-string
                   (lambda (p)
                     (parameterize ((current-output-port p))
                       (debugf-limited "test-key-1" 2 1 "limited ~a" "first")
                       (debugf-limited "test-key-1" 2 1 "limited ~a" "second")
                       (debugf-limited "test-key-1" 2 1 "limited ~a" "third"))))))
      (assert-eq output "limited firstlimited second" "debugf-limited output"))
    ;; Test debug-once
    (let ((output (call-with-output-string
                   (lambda (p)
                     (parameterize ((current-output-port p))
                       (debug-once "test-key-2" 1 "once ~a" 1)
                       (debug-once "test-key-2" 1 "once ~a" 2))))))
      (assert-eq output "once 1" "debug-once output"))
    ;; Restore original level
    (set-debug-level! original-level)
    #t))

(define (run-all-utils-tests)
  (let ((tests (list
                test-string-contains
                test-string-join-list
                test-string-split-char
                test-debug-log-levels
                test-format-string
                test-wire-or-list
                test-log-output-functions)))
    (display "Running utils tests...\n")
    (for-each (lambda (t)
                (format-string #t " - ~a ... " (format-string #f "~a" t))
                (let ((res (t)))
                  (if res
                      (display "ok\n")
                      (display "FAIL\n"))))
              tests)
    (display "All utils tests completed.\n")
    #t))

(run-all-utils-tests)
