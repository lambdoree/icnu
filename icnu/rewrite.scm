(define-module (icnu rewrite)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu icnu)
  #:export (rewrite-pass-A!
            rewrite-pass-copy-fold!
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

;;; ----------------------------------------------------------------
;;; 재작성 규칙
;;; ----------------------------------------------------------------

;; Applicator-Applicator(AA) 상호작용 규칙을 적용합니다. 두 Applicator 노드를 하나의 새로운 Applicator로 축약합니다.
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
            (delete-node! net b-name)
            (debugf 1 "rule-AA!: applied on ~a and ~a, created ~a\n" a-name b-name n_sym)
            #t)))))

;; Applicator-Copier(AC) 상호작용 규칙을 적용합니다. Copier를 통해 Applicator의 보조 포트들을 연결하고 Copier를 제거합니다.
(define (rule-AC! net a-name c-name)
  (if (and (eq? (node-agent net a-name) 'A) (eq? (node-agent net c-name) 'C))
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

;; Applicator-Eraser(AE) 상호작용 규칙을 적용합니다. Applicator와 Eraser를 제거합니다.
(define (rule-AE! net a-name e-name)
  (if (and (eq? (node-agent net a-name) 'A) (eq? (node-agent net e-name) 'E))
    (let ((e-p-peer (peer net (cons e-name 'p))))
      (when (equal? e-p-peer (cons a-name 'p))
        (delete-node! net a-name)
        (delete-node! net e-name)
        #t))
    #f))

;; Copier-Eraser(CE) 상호작용 규칙을 적용합니다. Copier와 Eraser를 제거합니다.
(define (rule-CE! net c-name e-name)
  (if (and (eq? (node-agent net c-name) 'C) (eq? (node-agent net e-name) 'E))
    (let ((e-p-peer (peer net (cons e-name 'p))))
      (when (equal? e-p-peer (cons c-name 'p))
        (delete-node! net c-name)
        (delete-node! net e-name)
        #t))
    #f))

;; 주어진 노드 이름이 상수 폴딩을 위한 리터럴 토큰으로 취급되어야 하는지 확인합니다.
(define (is-literal-node? net node-name)
  "Return #t for nodes that should be treated as literal tokens for folding:
   - canonical literal names (lit-*, num-, str-, trig-*)
   - prefixed true/false tokens (true*, false*)
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
             ;; A node with no aux peers is often a token-like literal.
             (and (eq? (node-agent net node-name) 'A)
                  (not (peer net (cons node-name 'l)))
                  (not (peer net (cons node-name 'r)))
                  (not (string-prefix? "eq-" s))
                  (not (string-prefix? "lt-" s))
                  (not (string-prefix? "gt-" s))
                  (not (string-prefix? "if-impl" s)))))))

;; 리터럴 형태의 노드 이름에서 Scheme 값을 추출합니다.
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
;; Returns #f when no literal found within `limit` steps.
;; 엔드포인트(`ep`)를 따라가서 연결된 리터럴 값을 찾습니다. Copier나 passthrough 노드를 투과하여 추적합니다.
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
                         ;; It's a literal node, get its value.
                         ((is-literal-node? net n) (get-literal-value n))
                         ;; It's an Applicator (A): check for boolean gadget, passthrough, or cell patterns.
                         ((eq? agent 'A)
                          (let ((s (symbol->string n)))
                            (if (or (string-contains? s "eq") (string-contains? s "lt")
                                    (string-contains? s "gt") (string-prefix? "if-impl" s))
                                *unresolved* ;; It's an operator, don't trace through it.
                                (let* ((r-peer (peer net (cons n 'r)))
                                       (l-peer (peer net (cons n 'l)))
                                       (r-agent (and r-peer (node-agent net (car r-peer))))
                                       (l-agent (and l-peer (node-agent net (car l-peer)))))
                                  (cond
                                    ((eq? r-agent 'E) #t)
                                    ((eq? l-agent 'E) #f)
                                    ;; Passthrough A node (no aux ports): follow its principal port.
                                    ((and (not l-peer) (not r-peer))
                                     (let ((p-peer (peer net (cons n 'p))))
                                       (if p-peer (loop p-peer (- k 1) seen) *unresolved*)))
                                    ;; Cell or wrapper with value on .r: follow it.
                                    (r-peer (loop r-peer (- k 1) seen))
                                    (else *unresolved*))))))
                         ;; It's a Copier (C) node: follow its principal port.
                         ((eq? agent 'C)
                          (let ((p-peer (peer net (cons n 'p))))
                            (if p-peer (loop p-peer (- k 1) seen) *unresolved*)))
                         ;; Not a traceable node, so we can't resolve a literal.
                         (else *unresolved*)))
                     ;; The endpoint isn't a valid (node . port) pair.
                     *unresolved*)))))))))

;;; ----------------------------------------------------------------
;;; 재작성 패스
;;; ----------------------------------------------------------------

;; 넷 전체에 대해 활성 쌍(active pair)을 찾아 `rule-AA!`를 적용하는 재작성 패스입니다.
(define (rewrite-pass-A! net)
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (b . 'A))
          (let* ((s-a (symbol->string a)) (s-b (symbol->string b))
                 (pass-through? (or (string-prefix? "inj-" s-a) (string=? "out" s-a) (string-prefix? "surface-merger-" s-a) (string-prefix? "a-" s-a)
                                    (string-prefix? "inj-" s-b) (string=? "out" s-b) (string-prefix? "surface-merger-" s-b) (string-prefix? "a-" s-b))))
            (when (rule-AA! net a b pass-through?)
              (set! changed? #t))))
         (_ #f)))
     pairs)
    changed?))

;; 넷 전체에 대해 활성 쌍을 찾아 AC, AE, CE 규칙을 적용하여 Copier 관련 상호작용을 처리하는 재작성 패스입니다.
(define (rewrite-pass-copy-fold! net)
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (c . 'C))
          (when (rule-AC! net a c)
            (debugf 1 "rule-AC!: applied on ~a and ~a\n" a c)
            (set! changed? #t)))
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

;; 넷에 주어진 boolean 값에 대한 전역 리터럴 노드가 없으면 생성하고, 그 이름을 반환합니다.
(define (ensure-global-bool-node net val)
  (let ((name (if val (gensym "lit-true-") (gensym "lit-false-"))))
    (add-node! net name 'A)
    (mark-nu! net name)
    name))

;; 조건이 리터럴 boolean 값으로 결정되는 IF 노드를 축약(fold)하는 재작성 패스입니다.
(define (rewrite-pass-if-fold! net)
  "Fold IF nodes when their condition resolves to a literal boolean via
   shallow tracing (resolve-literal-ep). This allows patterns like
   trigger -> in-copy -> condition to be recognized as constants."
  (let ((changed? #f))
    (for-each
     (lambda (if-name)
       (when (string-prefix? "if-impl-" (symbol->string if-name))
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

;; 비교 연산자 가젯의 입력이 모두 리터럴 값으로 결정될 때, 이를 미리 계산하여 상수화하는 재작성 패스입니다.
(define (rewrite-pass-const-fold! net)
  "Perform local constant folding for small comparator/const patterns.
   Use a shallow endpoint resolver (resolve-literal-ep) to follow common
   passthroughs (C copy nodes, passthrough A nodes) so comparator inputs
   that are fed via copier/trigger chains are still recognized as literals."
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

;; 경량 정리 패스: 연결된 포트가 없는 고아 Copier(C) 노드를 제거합니다.
(define (rewrite-pass-wire-cleanup! net)
  "Lightweight cleanup pass: remove orphaned copier (C) nodes that have no
   connected ports. This avoids build-up of dead scaffolding created during
   transformations and keeps subsequent passes efficient."
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

;; A-A 병합을 위한 재작성 패스입니다.
(define (rewrite-pass-AA-merge! net)
  "Merge adjacent Applicator (A) active pairs when possible by applying rule-AA!.
   Returns #t if the net was changed."
  (let ((changed? #f)
        (pairs (find-active-pairs net)))
    (for-each
     (lambda (pair)
       (match pair
         (((a . 'A) (b . 'A))
          ;; Try to apply AA merge; do not treat as pass-through here so that
          ;; rule-AA! performs a true merge when possible.
          (when (rule-AA! net a b #f)
            (set! changed? #t)))
         (_ #f)))
     pairs)
    changed?))
