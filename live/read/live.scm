(define (match-char? k char)
  (cond ((procedure? k) (not (not (k char))))
        ((char? k) (eqv? k char))
        (else #f)))

(define (read-char? k)
  (and (match-char? k (peek-char))
       (begin (read-char))))

(define (read-char* k)
  (let* ((first-char (read-char? k))
         (chars (let ((out (open-output-string)))
                  (let loop ((char first-char))
                    (cond ((or (eqv? #f char) (eof-object? char))
                           (get-output-string out))
                          (else
                           (write-char char out)
                           (loop (read-char? k))))))))
    (if (= 0 (string-length chars)) #f chars)))
