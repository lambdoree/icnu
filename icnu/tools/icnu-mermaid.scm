(define-module (icnu tools icnu-mermaid)
  #:use-module (icnu icnu)
  #:use-module (icnu utils format)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils log)
  #:export (net->mermaid-string write-mermaid-file sanitize-id-ml))

;; Mermaid exporter for ICNU nets (flowchart).
;; 추가된 내용 요약 (한국어 주석):
;;  - net->mermaid-string : 주어진 net을 mermaid flowchart 형식의 문자열로 변환합니다.
;;  - write-mermaid-file  : net을 mermaid 문자열로 파일에 기록합니다.
;; 사용 예:
;;   (use-modules (icnu icnu) (icnu tools icnu-mermaid))
;;   (display (net->mermaid-string net))
;;   (write-mermaid-file net "out.mmd")
;;
;; 주의:
;;  - Mermaid는 방향 그래프를 주로 사용하므로 "flowchart LR" (왼->오) 형식을 사용합니다.
;;  - interaction-nets의 링크는 무향이지만 시각화를 위해 "A -- \"p-r\" --> B" 형태의
;;    방향 에지(레이블 포함)를 사용합니다. 중복 에지는 내부적으로 제거됩니다.

(define (sanitize-id-ml s)
  "Mermaid에 안전한 식별자로 변환: 영숫자/밑줄만 남기고 나머지는 '_'로 치환, 앞에 n 접두어 추가"
  (let ((sstr (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
    (let loop ((chars (string->list sstr)) (out "n"))
      (if (null? chars)
          out
          (let ((c (car chars)))
            (let ((ok (or (char-numeric? c) (char-alphabetic? c) (char=? c #\_))))
              (loop (cdr chars)
                    (string-append out
                                   (if ok
                                       (string c)
                                       "_")))))))))

(define (escape-ml-label s)
  "Mermaid 라벨 내에서 큰따옴표를 이스케이프"
  (let ((str (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
    (string-replace str "\"" "\\\"")))

(define (node-label-for-ml net name)
  (let* ((agent (node-agent net name))
         (tag   (node-tag net name))
         (meta  (node-meta net name))
         (meta-str (if meta (format-string #f "~a" meta) "")))
    (format-string #f "~a\\nagent=~a\\ntag=~a~a"
                   name
                   (if agent agent "")
                   (if tag tag "user/opaque")
                   (if (string=? meta-str "") "" (format-string #f "\\nmeta=~a" (escape-ml-label meta-str))))))

(define (net->mermaid-string net)
  "Return a Mermaid flowchart string (flowchart LR) representing the given net.
This implementation is defensive: it converts all parts to strings explicitly
and avoids using dynamic format calls that may receive unexpected types."
  (let ((lines '()))
    (define (emit fmt . args)
      (set! lines (cons (apply format-string #f fmt args) lines)))
    (emit "flowchart LR")
    ;; Emit nodes: ensure id and label are strings
    (hash-for-each
     (lambda (name agent)
       (let* ((id (sanitize-id-ml name))
              (lbl (let ((raw (node-label-for-ml net name)))
                     (if (string? raw) raw (format-string #f "~a" raw)))))
         (emit "  ~a[\"~a\"]" id lbl)))
     (net-nodes net))
    ;; Emit edges, deduplicate by ordered key similar to dot exporter
    (let ((seen (make-hash-table)))
      (hash-for-each
       (lambda (ep peer)
         (when (and (pair? ep) (pair? peer))
           (let* ((a (car ep)) (pa (cdr ep))
                  (b (car peer)) (pb (cdr peer))
                  (ka (format-string #f "~a|~a" a pa))
                  (kb (format-string #f "~a|~a" b pb))
                  (key (if (and (symbol? a) (symbol? b)
                                (string<? (symbol->string a) (symbol->string b)))
                           (string-append ka "##" kb)
                           (string-append kb "##" ka))))
             (unless (hash-ref seen key #f)
               (hash-set! seen key #t)
               (let* ((sorted? (and (symbol? a) (symbol? b) (string<? (symbol->string a) (symbol->string b))))
                      (node1 (if sorted? a b))
                      (port1 (if sorted? pa pb))
                      (node2 (if sorted? b a))
                      (port2 (if sorted? pb pa))
                      (id1 (sanitize-id-ml node1))
                      (id2 (sanitize-id-ml node2))
                      (edg-lbl (format-string #f "~a-~a" port1 port2)))
                 (emit "  ~a -- \"~a\" --> ~a" id1 edg-lbl id2))))))
       (net-links net)))
    ;; join lines and return with trailing newline
    (string-append (string-join-list (reverse lines) "\n") "\n")))


(define (write-mermaid-file net path)
"Write mermaid string for NET into PATH. Returns PATH on success."
(let ((s (net->mermaid-string net)))
  (call-with-output-file path
    (lambda (port)
      (display s port)
      (force-output port))))
path)

;; Usage hint (for your shell): dot/mermaid rendering examples are provided in docs;
;; 예: 생성 후 로컬에서 mermaid-cli로 PNG 생성 가능 (추가 설치 필요):
;;   npx mmdc -i out.mmd -o out.png
;;
;; (위 커맨드는 시스템에 mermaid-cli(mmdc)를 설치한 경우에만 동작합니다.)
