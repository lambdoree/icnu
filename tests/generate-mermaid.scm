;; 목적: 대표적인 ICNU 예제들에 대해 small-step 시퀀스(및 big-step 최종 상태)를
;; Mermaid(flowchart LR) 파일로 출력하여 학습용으로 시각화합니다.
;;
;; 사용법:
;;   guile -L . tests/generate-mermaid.scm
;; 출력:
;;   tests/mermaid/<name>-step-000.mmd ... <name>-step-NNN.mmd
;;   tests/mermaid/<name>-bigstep.mmd
;;
;; 설계:
;;  - examples 리스트에 대표적인 sexpr 문자열들을 넣었습니다. 필요시 여기에 항목을 추가하세요.
;;  - 각 예제에 대해 small-step 시퀀스를 최대 max-steps 단계까지 생성하고,
;;    각 단계의 net을 Mermaid로 변환해 파일로 씁니다.
;;  - 또한 big-step (reduce-net-to-normal-form) 결과를 Mermaid로 출력합니다.
;;
;; 주의:
;;  - 이 스크립트는 테스트용(학습용)으로 설계되었으며, 대형 넷은 많은 파일을 생성할 수 있습니다.
;;  - Mermaid 렌더링(예: npx mmdc)은 로컬에서 별도로 설치해야 합니다.

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

;; ensure output directory exists
(define (ensure-out-dir)
  (let ((cmd (string-append "mkdir -p " out-dir)))
    (system cmd)
    #t))

;; helper: write a mermaid string for a net to a path (uses write-mermaid-file from icnu/tools/icnu-mermaid)
(define (write-net-mermaid net path)
  (write-mermaid-file net path)
  (format-string #t "wrote ~a~%" path)
  path)

;; helper: pretty name -> filesystem-safe name
(define (safe-name s)
  (let ((str (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
    (let loop ((chars (string->list str)) (acc ""))
      (if (null? chars)
          acc
          (let ((c (car chars)))
            (let ((ok (or (char-alphabetic? c) (char-numeric? c) (char=? c #\-) (char=? c #\_))))
              (loop (cdr chars) (string-append acc (if ok (string c) "_")))))))))

;; helper: zero-pad integer to width 3 as a string (avoids unsupported ~03d format)
(define (pad3 n)
  (let ((s (number->string n)))
    (cond ((< n 10) (string-append "00" s))
          ((< n 100) (string-append "0" s))
          (else s))))

;; produce small-step sequence (list of nets) using small-step-sequence-net defined in icnu/tools/icnu-proof
;; small-step-sequence-net returns list of nets (starting net ... final)
;; We'll cap max-steps to avoid explosion
(define (generate-and-write name sexpr-str max-steps)
  (format-string #t "Processing example: ~a~%" name)
  (let* ((sexpr (if (string? sexpr-str) (read-sexpr-from-string sexpr-str) sexpr-str))
         (net   (parse-net sexpr))
         (seq   (small-step-sequence-net (copy-net net) max-steps)))
    ;; write each step
    (let loop ((ns seq) (i 0))
      (when (pair? ns)
        (let* ((n (car ns))
               (fname (string-append out-dir "/" (safe-name name) "-step-" (pad3 i) ".mmd")))
          (write-net-mermaid n fname))
        (loop (cdr ns) (+ i 1))))
    ;; also write big-step result
    (let* ((big (reduce-net-to-normal-form (copy-net net) '((max-iter . 1000))))
           (bfname (string-append out-dir "/" (safe-name name) "-bigstep.mmd")))
      (write-net-mermaid big bfname))
	#t))

;; Representative examples (문자열은 tests 안의 예제와 유사하게 구성)
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

;; main
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

;; run when invoked
(main)
