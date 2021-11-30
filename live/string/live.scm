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

(define (string->one-char-strings string)
  (let loop ((n (string-length string))
             (strings '()))
    (if (zero? n)
        strings
        (let ((m (- n 1)))
          (loop m
                (cons (string-copy string m n)
                      strings))))))

;; SRFI 140 (Immutable Strings) has the following:
;;
;; (string-split string delimiter [grammar limit start end]) -> list

(define (string-split string delimiter)
  (if (string-null? delimiter)
      (string->one-char-strings string)
      (reverse
       (let loop ((parts '()) (a 0))
         (if (> a (string-length string))
             parts
             (let* ((b (string-contains string delimiter a))
                    (part (string-copy
                           string a (or b (string-length string))))
                    (parts (cons part parts)))
               (if b
                   (loop parts (+ b (string-length delimiter)))
                   parts)))))))

(define (with-input-from-string str proc)
  (call-with-port (open-input-string str)
                  (lambda (in)
                    (parameterize ((current-input-port in))
                      (proc)))))

(define (with-output-to-string proc)
  (call-with-port (open-output-string)
                  (lambda (out)
                    (parameterize ((current-output-port out))
                      (proc))
                    (get-output-string out))))
