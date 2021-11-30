#!r7rs
(define-library (live json shim)
  (export directory-list file-regular? arithmetic-shift)
  (import (scheme base)
          (scheme file))

  (import (only (racket) arithmetic-shift))

  (begin

    (define file-regular?
      (lambda (x)
        (guard (ex (else #f))
               (call-with-input-file x
                 (lambda _ #t)))))
    (define directory-list
      (lambda _
        (include "data-index.scm")))))
