(define (adler32-accumulator)
  (let ((sum 1))
    (lambda (bytevector)
      (unless (eof-object? bytevector)
        ;; Gauche's adler32 also takes strings, not only bytevectors.
        ;; What are the semantics -- does it use a predictable
        ;; character encoding?
        (typecheck-bytevector adler32-accumulator bytevector)
        (set! sum (gauche-adler32 bytevector sum)))
      sum)))

(define (adler32-bytevector bytevector)
  ((adler32-accumulator) bytevector))

(define (adler32-port in)
  (accumulate-bytevectors-from-port (adler32-accumulator) 4096 in))
