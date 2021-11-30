(define (vector-cons x vec)
  (vector-append (vector x) vec))

(define (vector-cons-right x vec)
  (vector-append vec (vector x)))

(define (vector-first vec)
  (vector-ref vec 0))

(define (vector-last-index vec)
  (if (zero? (vector-length vec)) #f (fx- (vector-length vec) 1)))

(define (vector-last vec)
  (vector-ref vec (vector-last-index vec)))

(define (vector<? elem<? vec1 vec2)
  (let ((len1 (vector-length vec1))
        (len2 (vector-length vec2)))
    (let loop ((i 0))
      (cond ((= i len1) (not (= i len2)))
            ((= i len2) #f)
            ((elem<? (vector-ref vec1 i) (vector-ref vec2 i)) #t)
            ((elem<? (vector-ref vec2 i) (vector-ref vec1 i)) #f)
            (else (loop (+ i 1)))))))
