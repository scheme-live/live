(define-library (live number)
  (export natural?)
  (import (scheme base))
  (begin

    (define (natural? obj)
      (and (integer? obj) (exact-integer? obj) (not (negative? obj))))))
