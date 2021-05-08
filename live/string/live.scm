(define (string-last-index str)
  (let ((n (string-length str)))
    (if (zero? n) #f (- n 1))))

(define (string-char-prefix? char str)
  (and (not (string-null? str))
       (char=? char (string-ref str 0))))

(define (string-char-suffix? char str)
  (let ((i (string-last-index str)))
    (and i (char=? char (string-ref str i)))))

(define (string-blank? str)
  (string-every char-whitespace? str))

(define (with-input-from-string str proc)
  (call-with-port (open-input-string str)
                  (lambda (in)
                    (parameterize ((current-input-port in))
                      (proc)))))
