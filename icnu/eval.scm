(define-module (icnu eval)
  #:use-module (srfi srfi-1)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:export (eval-icnu-string eval-net reduce-net-to-normal-form *default-reduction-passes* read-sexpr-from-string))

(define (read-sexpr-from-string s)
  (call-with-input-string s read))

(define *default-reduction-passes*
  (list rewrite-pass-copy-fold!
        rewrite-pass-const-fold!
        rewrite-pass-if-fold!
        rewrite-pass-AA-merge!
        rewrite-pass-wire-cleanup!))

(define (apply-reduction-passes! net passes)
  (let ((changed? #f))
    (for-each
     (lambda (pass)
       (when (pass net) (set! changed? #t)))
     passes)
    changed?))

(define (reduce-net-to-normal-form net . maybe-opts)
  "Reduce `net` to normal form.
   Options (opts alist):
     (max-iter . N)   -> N is a number: iteration cap (default 100)
     (max-iter . #f)  -> unlimited iterations (no iteration cap)
     (passes . passes) -> override default passes

   Note: we use `assoc` to detect presence of the max-iter entry so that an
   explicit (max-iter . #f) can be distinguished from the absence of the key."
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (max-entry (assoc 'max-iter opts))
         (max-iter (if max-entry (cdr max-entry) 100))
         (passes (let ((v (assq-ref opts 'passes))) (if v v *default-reduction-passes*))))
    (let loop ((i 0))
      ;; If max-iter is a number, enforce the cap; if it's #f (or any non-number),
      ;; treat as unlimited (no cap) and continue until fixpoint.
      (if (and (number? max-iter) (> i max-iter))
          (begin
            (warnf "reduce-net-to-normal-form: exceeded max iterations\n")
            net)
          (if (apply-reduction-passes! net passes)
              (loop (+ i 1))
              net)))
    net))

;; ------------------------------------------------------------
;; 옵션 유틸
;; ------------------------------------------------------------
(define (opt-ref opts key default)
  (let ((v (assq-ref opts key)))
    (if v v default)))

(define (common-ports) '(p r l))

(define (ports-excluding primary)
  (filter (lambda (p) (not (eq? p primary))) '(r l p)))

;; ------------------------------------------------------------
;; 리졸브 유틸
;; ------------------------------------------------------------

;; EP(노드.포트)에서 리터럴을 시도
(define (resolve-ep->literal net ep)
  (let ((res (resolve-literal-ep net ep)))
    (if (not (eq? res *unresolved*)) res *unresolved*)))

;; peer EP에서 일반 리졸브 → 직접 리터럴 판별까지 순차 시도
(define (resolve-peer-general-or-direct net peer-ep)
  (if (not peer-ep)
      *unresolved*
      (let* ((pn (car peer-ep))
             (res (resolve-literal-ep net peer-ep)))
        (cond
         ((not (eq? res *unresolved*)) res)
         ((and pn (is-literal-node? net pn)) (get-literal-value pn))
         ;; Check if the peer is a boolean gadget (A-node wired to an Eraser).
         ((and pn (eq? (node-agent net pn) 'A))
          (let* ((rpeer (peer net (cons pn 'r)))
                 (lpeer (peer net (cons pn 'l))))
            (cond
             ((and rpeer (eq? (node-agent net (car rpeer)) 'E)) #t)
             ((and lpeer (eq? (node-agent net (car lpeer)) 'E)) #f)
             (else *unresolved*))))
         (else *unresolved*)))))

;; A 노드 + E 지우개 패턴으로 boolean 추론 (#t/#f) 시도
;; 못 찾으면 *unresolved* 반환
(define (boolean-heuristic-A-E net node-name port)
  (let ((pp (peer net (endpoint node-name port))))
    (if (and pp (let ((pn (car pp))) (and pn (eq? (node-agent net pn) 'A))))
        (let* ((pn (car pp))
               (rpeer (peer net (cons pn 'r)))
               (lpeer (peer net (cons pn 'l))))
          (cond
           ((and rpeer (eq? (node-agent net (car rpeer)) 'E)) #t)
           ((and lpeer (eq? (node-agent net (car lpeer)) 'E)) #f)
           (else *unresolved*)))
        *unresolved*)))

;; ------------------------------------------------------------
;; out-name 기반 해석 파이프라인
;; ------------------------------------------------------------

;; 1) 주포트에서 먼저 시도
(define (try-primary-port net out-name out-port)
  (resolve-ep->literal net (endpoint out-name out-port)))

;; 2) 다른 흔한 포트(r l p, 단 주포트 제외)에서 순차 시도
(define (try-other-ports net out-name out-port)
  (let loop ((ps (ports-excluding out-port)))
    (if (null? ps)
        *unresolved*
        (let ((res (resolve-ep->literal net (endpoint out-name (car ps)))))
          (if (not (eq? res *unresolved*)) res (loop (cdr ps)))))))

;; 3) 최종 폴백(동일 포트 집합에 대해):
;;    3-1) 각 포트의 peer에 대해 일반 리졸브 → 직접 리터럴명 판별
;;    3-2) 그래도 실패하면 boolean A/E 휴리스틱
(define (final-fallback net out-name)
  (let loop2 ((ps2 (common-ports)))
    (if (null? ps2)
        ;; 3-2) boolean heuristic
        (let loop3 ((ps3 (common-ports)))
          (if (null? ps3)
              *unresolved*
              (let ((bh (boolean-heuristic-A-E net out-name (car ps3))))
                (if (not (eq? bh *unresolved*))
                    bh
                    (loop3 (cdr ps3))))))
        ;; 3-1) general-or-direct
        (let* ((pp (peer net (endpoint out-name (car ps2))))
               (res (resolve-peer-general-or-direct net pp)))
          (if (not (eq? res *unresolved*))
              res
              (loop2 (cdr ps2)))))))

;; out-name이 주어진 경우 전체 해석 시도
(define (resolve-from-out-name net out-name out-port)
  (let ((primary (try-primary-port net out-name out-port)))
    (if (not (eq? primary *unresolved*))
        primary
        (let ((other (try-other-ports net out-name out-port)))
          (if (not (eq? other *unresolved*))
              other
              (final-fallback net out-name))))))

;; ------------------------------------------------------------
;; 메인 엔트리
;; ------------------------------------------------------------
(define (extract-result-from-net net opts)
  (let* ((out-name    (assq-ref opts 'out-name))
         (out-port    (opt-ref opts 'out-port 'p))
         (result-form (opt-ref opts 'result-form 'string)))
    (cond
     (out-name
      (resolve-from-out-name net out-name out-port))
     ((eq? result-form 'net)
      net)
     (else
      (format-string #f "~a" (pretty-print net '((show-nu? . #t)))))))
  )

(define (eval-net net . maybe-opts)
  "Reduces a net to normal form and extracts a result."
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (reduced-net (reduce-net-to-normal-form net opts)))
    (extract-result-from-net reduced-net opts)))

(define (eval-icnu-string icnu-string . maybe-opts)
  "The main evaluation function. Parses a string, reduces it, and returns a result."
  (let* ((opts (if (null? maybe-opts) '() (car maybe-opts)))
         (sexpr (read-sexpr-from-string icnu-string))
         (net (parse-net sexpr)))
    (eval-net net opts)))
