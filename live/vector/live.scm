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
