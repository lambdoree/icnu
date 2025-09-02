(define-module (icnu utils log)
  #:use-module (icnu utils format)
  #:export (debug-level? set-debug-level! set-debug-log!
           debugf warnf debugf-limited debug-once
           icnu-debugf icnu-warnf icnu-debugf-limited icnu-debug-once))

(define *debug-level* (make-parameter 1))

(define (debug-level?) (*debug-level*))

(define (set-debug-level! n) (*debug-level* n))

(define (set-debug-log! v)
  (if (boolean? v)
      (set-debug-level! (if v 1 0))
      (set-debug-level! v)))

(define (debugf level fmt . args)
  (when (>= (debug-level?) level)
    (apply format-string (current-output-port) fmt args)
    (force-output (current-output-port))))

;; New wrapper with icnu- prefix
(define (icnu-debugf level fmt . args)
  (apply debugf level fmt args))

(define (warnf fmt . args)
  (apply debugf (cons 1 (cons fmt args))))

;; New wrapper with icnu- prefix
(define (icnu-warnf fmt . args)
  (apply warnf fmt args))

(define *debug-counts* (make-hash-table))

(define (debugf-limited key limit level fmt . args)
  (when (>= (debug-level?) level)
    (let ((kstr (if (symbol? key) (symbol->string key) (format-string #f "~a" key))))
      (let ((cnt (hash-ref *debug-counts* kstr 0)))
        (when (< cnt limit)
          (hash-set! *debug-counts* kstr (+ cnt 1))
          (apply format-string (current-output-port) fmt args)
          (force-output (current-output-port)))))))

;; New wrapper with icnu- prefix
(define (icnu-debugf-limited key limit level fmt . args)
  (apply debugf-limited key limit level fmt args))

(define (debug-once key level fmt . args)
  (apply debugf-limited (cons key (cons 1 (cons level (cons fmt args))))))

;; New wrapper with icnu- prefix
(define (icnu-debug-once key level fmt . args)
  (apply debug-once key level fmt args))


