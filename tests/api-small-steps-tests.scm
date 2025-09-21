;; Ensure project root is on %load-path when invoked as a script
(let* ((%script (or (current-filename) (car (program-arguments))))
       (dir-of (lambda (p)
                 (let ((n (string-length p)))
                   (let loop ((i (- n 1)))
                     (if (< i 0) "."
                         (if (char=? (string-ref p i) #\/)
                             (substring p 0 i)
                             (loop (- i 1))))))))
       (%here (dir-of %script))
       (%root (dir-of %here)))
  (set! %load-path (cons %root %load-path))
  (set! %load-compiled-path (cons %root %load-compiled-path)))

(define-module (tests api-small-steps)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module ((ice-9 pretty-print) #:prefix pp:)
  #:use-module (icnu utils format)
  #:use-module (icnu tools icnu-proof)
  #:export (main))

(define (read-sexp-string path)
  (call-with-input-file path
    (lambda (port)
      (let ((sexpr (read port)))
        (format-string #f "~a" sexpr)))))

(define (basename-no-ext path)
  (let* ((n (string-length path))
         (last-slash
          (let loop ((i (- n 1)) (pos -1))
            (if (< i 0) pos
                (if (char=? (string-ref path i) #\/)
                    i
                    (loop (- i 1) pos)))))
         (start (+ last-slash 1))
         (fname (substring path start n))
         (len (string-length fname)))
    (if (and (>= len 5)
             (string=? ".icnu" (substring fname (- len 5) len)))
        (substring fname 0 (- len 5))
        fname)))

(define (list-example-icnu-files)
  (let ((all (scandir "examples")))
    (map (lambda (name) (string-append "examples/" name))
         (filter (lambda (name)
                   (and (not (or (string=? name ".") (string=? name "..")))
                        (let ((ls (string-length name)))
                          (and (>= ls 5)
                               (string=? ".icnu" (substring name (- ls 5) ls))))))
                 all))))

(define (write-string-to-file s path)
  (call-with-output-file path
    (lambda (port)
      (display s port)
      (newline port)
      (force-output port)))
  path)

(define (pretty-sexpr-string s)
  (call-with-output-string
    (lambda (out)
      (pp:pretty-print (call-with-input-string s read) out))))

(define (write-seq-to-dir seq out-dir)
  (let loop ((lst seq) (i 1))
    (when (pair? lst)
      (let ((path (string-append out-dir "/step" (number->string i) ".icnu")))
        (write-string-to-file (pretty-sexpr-string (car lst)) path))
      (loop (cdr lst) (+ i 1)))))

(define (run file limit)
  (let* ((src (read-sexp-string file))
         (seq (small-step-sequence-string src limit))
         (base (basename-no-ext file))
         (out-dir (string-append "output/" base)))
    (system (string-append "mkdir -p " out-dir))
    (write-string-to-file (pretty-sexpr-string src) (string-append out-dir "/step0.icnu"))
    (write-seq-to-dir seq out-dir)
    #t))

(define (run-all limit)
  (for-each
   (lambda (f) (run f limit))
   (list-example-icnu-files))
  #t)

(define (main args)
  (match (cdr args)
    (()
     (run-all 100))
    ((arg1)
     (let* ((k (string->number arg1)))
       (if (and k (number? k))
           (run-all k)
           (run arg1 100))))
    ((file max)
     (let* ((k (string->number max))
            (limit (if (and (number? k) k) k 100)))
       (run file limit)))
    (else
     (run-all 100))))

(main (program-arguments))
