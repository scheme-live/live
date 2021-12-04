(define-library (live hello)
  (import (scheme base)
          (scheme write))
  (export hello)

  (begin
    (include "hello/body.scm")))
