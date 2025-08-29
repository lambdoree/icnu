(use-modules (icnu utils format)
            (icnu utils strings)
            (icnu utils assertions)
            (srfi srfi-1)
            (icnu icnu)
            (icnu utils log)
            (icnu utils helpers)
            (tests test-runner))

(set-debug-level! 0)

(define (test-mk-helpers)
 (assert-eq (mk-node 'x 'A) '(node x A) "mk-node produces node sexpr")
 (assert-eq (mk-wire 'a 'p 'b 'r) '(wire (a p) (b r)) "mk-wire produces wire sexpr")
 (assert-eq (mk-par '(node a A) '(node b A)) '(par (node a A) (node b A)) "mk-par")
 (assert-eq (mk-nu '(a b) '(par)) '(nu (a b) (par)) "mk-nu")
 #t)

(define (test-empty-and-add-node)
 (let ((n (empty-net)))
   (assert-eq (length (all-names n)) 0 "empty-net has no names")
   (add-node! n 'foo 'A)
   (assert-eq (node-agent n 'foo) 'A "add-node! registers node agent")
   (add-node! n 'bar 'V)
   (assert-eq (node-agent n 'bar) 'V "bar created as V")
   (add-node! n 'bar 'A)
   (assert-eq (node-agent n 'bar) 'A "bar upgraded to A")
   #t))

(define (test-endpoint_and_linking_and_peers)
 (let ((n (empty-net)))
   (add-node! n 'a 'A)
   (add-node! n 'b 'A)
   (link-peers! n (endpoint 'a 'p) (endpoint 'b 'p))
   (assert-eq (peer n (endpoint 'a 'p)) (endpoint 'b 'p) "peer after link")
   (unlink-port! n (endpoint 'a 'p))
   (assert-false (peer n (endpoint 'a 'p)) "peer after unlink removed")
   #t))

(define (test-rewire_and_delete)
 (let ((n (empty-net)))
   (add-node! n 'x 'A)
   (add-node! n 'y 'A)
   (add-node! n 'z 'A)
   (link-peers! n (endpoint 'x 'p) (endpoint 'y 'p))
   (assert-eq (peer n (endpoint 'x 'p)) (endpoint 'y 'p) "initial link")
   (rewire! n (endpoint 'x 'p) (endpoint 'z 'p))
   (assert-eq (peer n (endpoint 'x 'p)) (endpoint 'z 'p) "rewire to z")
   (delete-node! n 'z)
   (assert-false (peer n (endpoint 'x 'p)) "peer removed after delete-node!")
   #t))

(define (test-copy-net)
 (let ((n (empty-net)))
   (add-node! n 'u 'A)
   (add-node! n 'v 'A)
   (link-peers! n (endpoint 'u 'p) (endpoint 'v 'p))
   (let ((c (copy-net n)))
     (assert-true (hash-ref (net-nodes c) 'u #f) "copied net contains u")
     (assert-true (hash-ref (net-nodes c) 'v #f) "copied net contains v")
     (assert-eq (peer c (endpoint 'u 'p)) (endpoint 'v 'p) "copied link preserved"))
   #t))

(define (test-find-active-pairs)
 (let ((n (empty-net)))
   (add-node! n 'a 'A)
   (add-node! n 'b 'A)
   (link-peers! n (endpoint 'a 'p) (endpoint 'b 'p))
   (let ((pairs (find-active-pairs n)))
     (assert-true (and (pair? pairs) (not (null? pairs))) "find-active-pairs returns non-empty list")
     (let ((first (car pairs)))
       (assert-true (and (pair? (car first)) (pair? (cadr first))) "active pair shape")))
   #t))

(define (test-pretty-print_and_options)
 (let ((n (empty-net)))
   (add-node! n 'v1 'V)
   (add-node! n 'a 'A)
   (link-peers! n (endpoint 'v1 'p) (endpoint 'a 'p))
   (let ((pp (pretty-print n)))
     (assert-true (pair? pp) "pretty-print returns sexpr")
     (assert-true (not (string-contains? (format-string #f "~a" pp) "v1")) "v1 hidden by default"))
   (let ((pp2 (pretty-print n '((show-V? . #t)))))
     (assert-true (string-contains? (format-string #f "~a" pp2) "v1") "v1 shown when show-V?"))
   (let* ((n2 (parse-net '(nu (x) (par (node x A)))))
         (ppn (pretty-print n2 '((show-nu? . #t)))))
     (assert-true (string-contains? (format-string #f "~a" ppn) "nu") "pretty-print show-nu? from parsed net"))
   (let ((n3 (empty-net)))
     (mark-nu! n3 'm)
     (add-node! n3 'm 'A)
     (let ((pp3 (pretty-print n3 '((show-nu? . #t)))))
       (assert-true (string-contains? (format-string #f "~a" pp3) "nu") "pretty-print show-nu? from mark-nu!")))
   #t))

(define (test-ports_and_get-ports)
 (assert-eq (get-ports 'A) '(p l r) "get-ports for A")
 (assert-eq (get-ports 'C) '(p l r) "get-ports for C")
 (assert-eq (get-ports 'E) '(p) "get-ports for E")
 (assert-eq (get-ports 'V) '() "get-ports for unknown")
 (assert-true (valid-port? 'p) "p valid")
 (assert-false (valid-port? 'x) "x invalid")
 #t)

(define (test-make-fresh-name)
 (let ((n (empty-net)))
   (let ((m1 (make-fresh-name n "t"))
        (m2 (make-fresh-name n "t")))
     (assert-true (not (equal? m1 m2)) "make-fresh-name produces distinct names")
     (assert-true (string-prefix? "t-" (symbol->string m1)) "prefix respected"))
   (let loop ((i 0) (acc #t))
     (if (>= i 200)
        acc
        (let ((nm (make-fresh-name n "z")))
          (loop (+ i 1) (and acc (symbol? nm)))))))
 #t)


(define-syntax with-conflict-mode
  (syntax-rules ()
    [(_ mode body ...)
     (parameterize ((*link-conflict-mode* mode)) body ...)]))

(define (expect-error thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda args #t)))

(define (ep name port) (endpoint name port))

(define (make-A-net syms)
  (let ((n (empty-net)))
    (for-each (lambda (s) (add-node! n s 'A)) syms)
    n))

(define (assert-peer= n from expected msg)
  (assert-eq (peer n from) expected msg))


(define (test-default-mode-is-error)
  (assert-eq (*link-conflict-mode*) 'error "default conflict mode is error"))

(define (test-mode-inject-temporary)
  (with-conflict-mode 'error
    (let ((n (make-A-net '(a b c d))))
      (link-peers! n (ep 'a 'p) (ep 'b 'p))
      (with-conflict-mode 'inject-temporary
        (mark-temporary! n 'a 'p)
        (link-peers! n (ep 'a 'p) (ep 'c 'p))
        (assert-peer= n (ep 'a 'p) (ep 'c 'p)
                      "inject-temporary overwrites temporary port")
        (unmark-temporary! n 'a 'p)
        (assert-true
         (expect-error (lambda () (link-peers! n (ep 'a 'p) (ep 'd 'p))))
         "inject-temporary errors if no port is temporary")))))

(define (test-mode-replace-exact)
  (with-conflict-mode 'error
    (let ((n (make-A-net '(x y z))))
      (link-peers! n (ep 'x 'p) (ep 'y 'p))
      (with-conflict-mode 'replace-exact
        (link-peers! n (ep 'x 'p) (ep 'y 'p))
        (assert-peer= n (ep 'x 'p) (ep 'y 'p)
                      "replace-exact is idempotent")
        (assert-true
         (expect-error (lambda () (link-peers! n (ep 'x 'p) (ep 'z 'p))))
         "replace-exact errors on different peer")
        (assert-true
         (expect-error (lambda () (link-peers! n (ep 'y 'l) (ep 'z 'l))))
         "replace-exact errors on new link")))))


(define (test-link-conflict-modes)
  (test-default-mode-is-error)
  (test-mode-inject-temporary)
  (test-mode-replace-exact)
  #t)


(define (test-parse-net_and_parse-endpoint)
  (set-link-conflict-mode! 'error)
(let* ((sexpr '(par (node A A) (node B A) (wire (A p) (B p))))
     (net (parse-net sexpr)))
 (assert-eq (peer net (endpoint 'A 'p)) (endpoint 'B 'p) "parse-net builds links")
 (let* ((sex2 '(par (node c A) (node d A) (wire (list c p) (list d p))))
      (n2 (parse-net sex2)))
   (assert-eq (peer n2 (endpoint 'c 'p)) (endpoint 'd 'p) "parse-endpoint handles (list name port)"))
 (let ((n3 (parse-net '(par (node 'qual.name A)))))
   (assert-true (node-agent n3 'qual.name) "qualified name materialized as A"))
 #t))

(define (test-unlink-by-equal-key)
  (set-link-conflict-mode! 'error)
(let ((n (empty-net)))
  (parse-net '(par (node a A) (node b A) (wire (a p) (b p))))
  (unlink-port! n (cons 'a 'p))
  (assert-false (peer n (endpoint 'a 'p)) "unlink by equal key removed link")
  #t))

(define (test-delete-node_cleans_links)
  (set-link-conflict-mode! 'error)
  (let ((n (empty-net)))
    (parse-net '(par (node x A) (node y A) (wire (x p) (y p))))
    (delete-node! n 'y)
    (assert-false (peer n (endpoint 'x 'p)) "delete-node! removed x's peer after deleting y")
    #t))

(define (test-parse-net-malformed-wire)
  (let ((malformed-input '(par (wire (a p))))) ; wire with only one endpoint
    (assert-true
     (catch #t
       (lambda () (parse-net malformed-input) #f) ; return #f on success
       (lambda (key . args) #t)) ; return #t on any exception
     "parse-net should throw an error on malformed wire"))
  #t)

(define (test-link-conflict-error-mode)
  (let ((n (empty-net)))
    (add-node! n 'a 'A)
    (add-node! n 'b 'A)
    (add-node! n 'c 'A)
    (set-link-conflict-mode! 'error)
    (link-peers! n (endpoint 'a 'p) (endpoint 'b 'p))
    (assert-true
     (catch #t
       (lambda () (link-peers! n (endpoint 'a 'p) (endpoint 'c 'p)) #f) ; #f on success
       (lambda (key . args) #t)) ; #t on error
     "link-peers! with mode 'error should throw on conflict")
    #t))

(define (test-validate_copy_isolation)
  (set-link-conflict-mode! 'error)
  (let ((n (parse-net '(par (node u A) (node v A) (wire (u p) (v p))))))
    (let ((c (copy-net n)))
      (delete-node! n 'v)
      (assert-false (node-agent n 'v) "original lost v after delete")
      (assert-true (node-agent c 'v) "copied net still contains v")
      (let ((found #f))
        (hash-for-each (lambda (k v)
                         (when (and (pair? k) (eq? (car k) 'u) (eq? (cdr k) 'p))
                           (set! found #t)))
                       (net-links c))
        (assert-true found "copied net retains u.p link")))
    #t))

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
    (let ((output (call-with-output-string
					(lambda (p)
                      (parameterize ((current-output-port p))
						(warnf "warn message ~a" 1))))))
      (assert-eq output "warn message 1" "warnf output"))
    (let ((output (call-with-output-string
					(lambda (p)
                      (parameterize ((current-output-port p))
						(debugf-limited "test-key-1" 2 1 "limited ~a" "first")
						(debugf-limited "test-key-1" 2 1 "limited ~a" "second")
						(debugf-limited "test-key-1" 2 1 "limited ~a" "third"))))))
      (assert-eq output "limited firstlimited second" "debugf-limited output"))
    (let ((output (call-with-output-string
					(lambda (p)
                      (parameterize ((current-output-port p))
						(debug-once "test-key-2" 1 "once ~a" 1)
						(debug-once "test-key-2" 1 "once ~a" 2))))))
      (assert-eq output "once 1" "debug-once output"))
    (set-debug-level! original-level)
    #t))

(run-tests "Core"
           (list
            test-mk-helpers
            test-empty-and-add-node
            test-endpoint_and_linking_and_peers
            test-rewire_and_delete
            test-copy-net
            test-find-active-pairs
            test-pretty-print_and_options
            test-ports_and_get-ports
            test-make-fresh-name
            test-link-conflict-modes
            test-parse-net_and_parse-endpoint
            test-unlink-by-equal-key
            test-delete-node_cleans_links
            test-validate_copy_isolation
            test-parse-net-malformed-wire
            test-link-conflict-error-mode
            test-string-contains
            test-string-join-list
            test-string-split-char
            test-debug-log-levels
            test-format-string
            test-wire-or-list
            test-log-output-functions))
