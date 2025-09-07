(define-module (icnu utils strings)
  #:export (string-contains? string-join-list string-split-char icnu-string-replace
           icnu-string-contains? icnu-string-join-list icnu-string-split-char))

(define (string-contains? str substr)
  (let ((slen (string-length str))
        (sublen (string-length substr)))
    (if (> sublen slen)
        #f
        (let loop ((i 0))
          (if (> (+ i sublen) slen)
              #f
              (if (string=? substr (substring str i (+ i sublen)))
                  #t
                  (loop (+ i 1))))))))

;; New wrapper with icnu- prefix
(define (icnu-string-contains? str substr)
  (string-contains? str substr))

(define (string-join-list strings delimiter)
  (if (null? strings)
      ""
      (let loop ((result (car strings)) (rest (cdr strings)))
        (if (null? rest)
            result
            (loop (string-append result delimiter (car rest)) (cdr rest))))))

;; New wrapper with icnu- prefix
(define (icnu-string-join-list strings delimiter)
  (string-join-list strings delimiter))

(define (string-split-char s delim-char)
  (let loop ((i 0)
             (start 0)
             (acc '()))
    (if (>= i (string-length s))
        (reverse (cons (substring s start) acc))
        (if (char=? (string-ref s i) delim-char)
            (loop (+ i 1)
                  (+ i 1)
                  (cons (substring s start i) acc))
            (loop (+ i 1) start acc)))))

;; string-replace: replace all occurrences of a single-character delimiter `old`
;; with the string `new`. Accepts `old` as a string of length 1 or a char.
(define (icnu-string-replace s old new)
  (let ((delim-char (cond
                     ((char? old) old)
                     ((string? old)
                      (if (= (string-length old) 1)
                          (string-ref old 0)
                          (error "string-replace: old delimiter must be single character" old)))
                     (else (error "string-replace: old must be char or single-char string" old)))))
    (string-join-list (string-split-char s delim-char) new)))

;; New wrapper with icnu- prefix
(define (icnu-string-split-char s delim-char)
  (string-split-char s delim-char))

