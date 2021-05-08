(define-library (live list)
  (export
   ;; Re-exported from SRFI 1:
   any
   drop-while
   every
   fold
   unfold
   ;; Defined in this library:
   map/odd)
  (import (scheme base) (srfi 1))
  (begin

    (define (map/odd f xs)
      (let loop ((acc '()) (xs xs) (odd? #f))
        (if (null? xs) (reverse acc)
            (loop (cons (f (car xs) odd?)
                        acc)
                  (cdr xs)
                  (not odd?)))))))
