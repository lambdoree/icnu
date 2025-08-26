(define-module (icnu rewrite)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu icnu)
  #:export (rewrite-pass-copy-fold!
            rewrite-pass-if-fold!
            rewrite-pass-const-fold!
            rewrite-pass-wire-cleanup!
            rewrite-pass-AA-merge!
            resolve-literal-ep
            is-literal-node?
            get-literal-value
            *unresolved*))

;; Sentinel used by resolvers to indicate an unresolved / uncomputable result.
;; Use a stable symbol instead of #f to avoid conflation with boolean false.
(define *unresolved* (string->symbol "icnu-unresolved"))

;; How many steps resolver may follow when searching for a literal.
;; Increase if composed stdlib helpers or nested ν wrappers make resolution deeper.
;; Raised from 32 to 512 to avoid spurious *unresolved* results in composed nets.
(define *resolve-literal-limit* 512)

(define (rule-AA! net a-name b-name pass-through?)
  (when (and (eq? (node-agent net a-name) 'A)
             (eq? (node-agent net b-name) 'A))
    (debugf 1 "rule-AA!: attempting on ~a and ~a\n" a-name b-name)
    (let* ((a_p (cons a-name 'p)) (a_l (cons a-name 'l)) (a_r (cons a-name 'r))
           (b_p (cons b-name 'p)) (b_l (cons b-name 'l)) (b_r (cons b-name 'r))
           (X (peer net a_r)) (F (peer net b_l)) (Y (peer net b_r))
           (a-is-lit (is-literal-node? net a-name))
           (b-is-lit (is-literal-node? net b-name)))
      (cond
       (pass-through?
        (debugf 2 "rule-AA!: skipping on ~a and ~a (pass-through: ~a)\n" a-name b-name pass-through?)
        #f)
       ((or a-is-lit b-is-lit)
        #f)
       (else
        ;; Original logic for non-literals or two literals
        (let* ((n_sym (make-fresh-name net "a"))
               (n_p (cons n_sym 'p)) (n_l (cons n_sym 'l)) (n_r (cons n_sym 'r)))
          (add-node! net n_sym 'A)
          (when (or (hash-ref (net-nu-set net) a-name #f)
                    (hash-ref (net-nu-set net) b-name #f))
            (mark-nu! net n_sym))
          (if F (rewire! net a_l F) (unlink-port! net a_l))
          (when Y (rewire! net n_l Y))
          (when X (rewire! net n_r X))
          (rewire! net a_p n_p)
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
                  ;; an A-node wired to another A-node is an applicator, not a literal,
                  ;; unless that peer is just an output sink.
                  (let ((p-peer (peer net (cons node-name 'p))))
                    (if p-peer
                        (let* ((peer-name (car p-peer))
                               (peer-agent (node-agent net peer-name)))
                          (not (and (eq? peer-agent 'A)
                                    (not (string-prefix? "out" (symbol->string peer-name))))))
                        #t))
                  (not (string-prefix? "eq-" s))
                  (not (string-prefix? "lt-" s))
                  (not (string-prefix? "gt-" s))
                  (not (string-prefix? "if-impl" s))
                  ;; exclude common output/result node names from the heuristic
                  (not (string-prefix? "out" s)))))))

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

(define (ep-key ep)
  "엔드포인트 ep 를 순환 방지 key 문자열로 변환."
  (if (and (pair? ep) (symbol? (car ep)) (symbol? (cdr ep)))
      (string-append (symbol->string (car ep)) "|" (symbol->string (cdr ep)))
      (format-string #f "~a" ep)))

(define (operator-like-name? s)
  "A-노드 이름이 비교/분기 구현처럼 보이면 #t: eq/lt/gt 포함, if-impl* 접두."
  (or (string-contains? s "eq")
      (string-contains? s "lt")
      (string-contains? s "gt")
      (string-prefix? "if-impl" s)))

(define (peers-of net n)
  "노드 n 의 p/l/r 피어 엔드포인트들을 (values p l r) 로 반환."
  (values (peer net (cons n 'p))
          (peer net (cons n 'l))
          (peer net (cons n 'r))))

(define (boolean-from-eraser? net l-peer r-peer)
  "A-노드에서 Eraser 패턴으로 boolean 추론 (#t/#f)하거나, 아니면 'unknown."
  (cond
   ((and r-peer (eq? (node-agent net (car r-peer)) 'E)) #t)
   ((and l-peer (eq? (node-agent net (car l-peer)) 'E)) #f)
   (else 'unknown)))

(define (follow-port-peer net n port)
  "노드 n 의 지정 port('p/'l/'r) 피어 엔드포인트(있으면) 반환, 없으면 #f."
  (case port
    ((p) (peer net (cons n 'p)))
    ((l) (peer net (cons n 'l)))
    ((r) (peer net (cons n 'r)))
    (else #f)))

;; -----------------------------------------------------------------------------
;; Agent-specific resolvers (call with a tail 'recur' for recursion)
;; -----------------------------------------------------------------------------

(define (resolve-from-literal-node net n)
  "리터럴 노드 n 의 값."
  (get-literal-value n))

(define (resolve-from-A-node net n current-port k seen recur)
  "A-노드에서 리터럴 추론 시도:
   - 이름이 연산/분기 구현처럼 보이면 포기
   - Eraser 휴리스틱 (#t/#f)
   - 현재 진입 포트에 따라 선형 추적
   - l/r 없고 p만 있는 패스-스루"
  (let* ((s (symbol->string n)))
    (if (operator-like-name? s)
        *unresolved*
        (call-with-values
            (lambda () (peers-of net n))
          (lambda (p-peer l-peer r-peer)
            (let ((b (boolean-from-eraser? net l-peer r-peer)))
              (cond
               ((eq? b #t) #t)
               ((eq? b #f) #f)
               (else
                (let ((next (follow-port-peer net n current-port)))
                  (cond
                   (next (recur next (- k 1) seen))
                   ((and (not l-peer) (not r-peer) p-peer) ; pass-through
                    (recur p-peer (- k 1) seen))
                   (else *unresolved*)))))))))))

(define (resolve-from-C-node net n k seen recur)
  "C-노드는 p→l→r 순서로 첫 해석 성공을 반환."
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

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(define (resolve-literal-ep net ep . maybe-limit)
  "엔드포인트 ep 로부터 리터럴 값을 해석하여 반환.
   - 순환 방지를 위해 방문 집합(seen)을 사용
   - 최대 추적 횟수 limit (기본: *resolve-literal-limit*)"
  (let ((limit (if (null? maybe-limit) *resolve-literal-limit* (car maybe-limit))))
    (letrec
        ((recur
          (lambda (current-ep k seen)
            (cond
             ((or (not current-ep) (zero? k)) *unresolved*)
             (else
	      (let ((key (ep-key current-ep)))
                (if (hash-ref seen key #f)
                    *unresolved*
                    (begin
		      (hash-set! seen key #t)
		      (if (and (pair? current-ep) (symbol? (car current-ep)))
                          (let* ((n      (car current-ep))
                                 (port   (and (symbol? (cdr current-ep)) (cdr current-ep)))
                                 (agent  (node-agent net n)))
                            (cond
                             ((is-literal-node? net n)
                              (resolve-from-literal-node net n))
                             ((eq? agent 'A)
                              (resolve-from-A-node net n port k seen recur))
                             ((eq? agent 'C)
                              (resolve-from-C-node net n k seen recur))
                             (else *unresolved*)))
                          *unresolved*)))))))))
      (recur ep limit (make-hash-table)))))


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
                ;; attempt to resolve condition value (follow up to *resolve-literal-limit* steps)
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
       (let ((s (symbol->string n)))
         ;; match comparator-like names robustly: prefix/suffix or embedded markers
         (when (or (string-contains? s "eq") (string-contains? s "lt") (string-contains? s "gt"))
           (let* ((l-ep (peer net (cons n 'l)))
                  (r-ep (peer net (cons n 'r)))
                  ;; resolve values by following endpoints up to depth 8
                  ;; Use explicit conditional so that a missing aux endpoint
                  ;; yields the sentinel *unresolved* rather than #f, which
                  ;; would be ambiguous with the literal boolean #f.
                  (l-val (if l-ep (resolve-literal-ep net l-ep *resolve-literal-limit*) *unresolved*))
                  (r-val (if r-ep (resolve-literal-ep net r-ep *resolve-literal-limit*) *unresolved*)))
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
