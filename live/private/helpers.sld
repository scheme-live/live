(define-library (live private helpers)
  (export unwind-protect)
  (import (scheme base))
  (begin

    ;; Like dynamic-wind, but makes sure the cleanup is run only once.
    ;; From Common Lisp.
    (define (unwind-protect proc cleanup)
      (dynamic-wind
        (lambda () #f)
        proc
        (let ((called? #f))
          (lambda ()
            (unless called?
              (set! called? #t)
              (cleanup))))))))
