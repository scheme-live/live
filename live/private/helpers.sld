(define-library (live private helpers)
  (export unwind-protect)
  (import (scheme base))
  (begin

    ;; Like dynamic-wind, but ensures the `after` procedure is run
    ;; only once. From Common Lisp.
    (define (unwind-protect thunk after)
      (dynamic-wind
        (lambda () #f)
        thunk
        (let ((after? #t))
          (lambda ()
            (when after?
              (set! after? #f)
              (after))))))))
