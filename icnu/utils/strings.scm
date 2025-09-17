(define-module (icnu utils strings)
  #:export (string-contains? string-join-list string-split-char string-replace-char))

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

(define (string-join-list strings delimiter)
  (if (null? strings)
      ""
      (let loop ((result (car strings)) (rest (cdr strings)))
        (if (null? rest)
            result
            (loop (string-append result delimiter (car rest)) (cdr rest))))))

(define (string-split-char s delim-char)
  (let loop ((i 0)
             (start 0)
             (acc '()))
    (if (>= i (string-length s))
        (reverse (cons (substring s start (string-length s)) acc))
        (if (char=? (string-ref s i) delim-char)
            (loop (+ i 1)
                  (+ i 1)
                  (cons (substring s start i) acc))
            (loop (+ i 1) start acc)))))

(define (string-replace-char s old new)
  (let ((delim-char (cond
                     ((char? old) old)
                     ((string? old)
                      (if (= (string-length old) 1)
                          (string-ref old 0)
                          (error "string-replace-char: old delimiter must be single character" old)))
                     (else (error "string-replace-char: old must be char or single-char string" old)))))
    (string-join-list (string-split-char s delim-char) new)))


