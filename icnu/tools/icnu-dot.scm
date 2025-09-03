(define-module (icnu tools icnu-dot)
  #:use-module (icnu icnu)
  #:use-module (icnu utils format)
  #:use-module (icnu utils strings)
  #:use-module (icnu utils log)
  #:export (net->dot-string write-dot-file sanitize-id))

;; Simple Graphviz DOT exporter for ICNU nets.
;; Provides:
;;  - net->dot-string : produce a DOT representation (string) for visualizing nets
;;  - write-dot-file  : write a DOT file for a given net path
;;
;; Notes:
;;  - Produces an undirected "strict graph" (interaction nets links are symmetric).
;;  - Node labels include name, agent and tag/meta if present.
;;  - Edges are deduplicated by ordering endpoint names so each link is emitted once.

(define (sanitize-id s)
  "Produce a DOT-safe identifier from a symbol name string."
  (let ((sstr (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
    (let loop ((chars (string->list sstr)) (out "n"))
      (if (null? chars)
          out
          (let ((c (car chars)))
            (let ((ok (or (char-numeric? c)
                          (char-alphabetic? c)
                          (char=? c #\_))))
              (loop (cdr chars)
                    (string-append out
                                   (if ok
                                       (string c)
                                       "_")))))))))

(define (escape-label s)
  (let ((str (if (symbol? s) (symbol->string s) (format-string #f "~a" s))))
    ;; escape double quotes and backslashes
    (string-replace (string-replace str "\\" "\\\\") "\"" "\\\"")))

(define (endpoint->str ep)
  (if (pair? ep)
      (format-string #f "(~a.~a)" (car ep) (cdr ep))
      (format-string #f "~a" ep)))

(define (node-label-for net name)
  (let* ((agent (node-agent net name))
         (tag   (node-tag net name))
         (meta  (node-meta net name))
         (meta-str (if meta (format-string #f "~a" meta) "")))
    (format-string #f "%a\\nagent=%a\\ntag=%a%a"
                   name
                   (if agent agent "")
                   (if tag tag "user/opaque")
                   (if (string=? meta-str "") "" (format-string #f "\\nmeta=%a" (escape-label meta-str))))))

(define (net->dot-string net)
  "Return a Graphviz DOT representation (string) for the given net."
  (call-with-output-string
   (lambda (port)
     (format-string port "strict graph icnu_net {~%")
     (format-string port "  node [shape=box, fontname=\"Helvetica\"];~%")
     ;; Emit nodes
     (hash-for-each
      (lambda (name agent)
        (let ((id (sanitize-id name))
              (lbl (escape-label (node-label-for net name))))
          (format-string port "  ~a [label=\"~a\"];~%" id lbl)))
      (net-nodes net))
     ;; Emit edges (deduplicate by ordering pair of endpoint names)
     (let ((seen (make-hash-table)))
       (hash-for-each
        (lambda (ep peer)
          (when (and (pair? ep) (pair? peer))
            (let* ((a (car ep)) (pa (cdr ep))
                   (b (car peer)) (pb (cdr peer))
                   (ka (format-string #f "~a|~a" a pa))
                   (kb (format-string #f "~a|~a" b pb))
                   (key (if (string<? (symbol->string a) (symbol->string b))
                            (cons ka kb)
                            (cons kb ka))))
              (unless (hash-ref seen key #f)
                (hash-set! seen key #t)
                (let ((ida (sanitize-id a))
                      (idb (sanitize-id b))
                      (edg-lbl (format-string #f "~a-~a" pa pb)))
                  (format-string port "  ~a -- ~a [label=\"~a\"];~%" ida idb edg-lbl)))))))
        (net-links net)))
     (format-string port "}~%"))))

(define (write-dot-file net path)
  "Write DOT string for NET into PATH. Returns PATH on success."
  (let ((port (open-output-file path)))
    (unwind-protect
        (format port "~a" (net->dot-string net))
      (close-port port)))
  path)
