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

(define-module (tests api-eval)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 pretty-print) #:prefix pp:)
  #:use-module (icnu utils format)
  #:use-module (icnu tools icnu-big-step)
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

(define (run file)
  (let* ((src (read-sexp-string file))
         (out (big-step-output src))
         (base (basename-no-ext file))
         (out-dir (string-append "output/" base)))
    (system (string-append "mkdir -p " out-dir))
    (write-string-to-file (pretty-sexpr-string src) (string-append out-dir "/step0.icnu"))
    (write-string-to-file (pretty-sexpr-string out) (string-append out-dir "/step1.icnu"))
    #t))

(define (main args)
  (match (cdr args)
    (()
     (run "examples/l2-if-true.icnu"))
    ((arg1)
     (let* ((len (string-length arg1))
            (is-icnu (and (>= len 5) (string=? ".icnu" (substring arg1 (- len 5) len)))))
       (if is-icnu
           (run arg1)
           (run "examples/l2-if-true.icnu"))))
    ((file . _)
     (run file))
    (else
     (run "examples/l2-if-true.icnu"))))

(main (program-arguments))
