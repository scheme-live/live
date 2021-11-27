#!r7rs
(define-library (live hello)
  (export hello)

  (import (scheme base)
          (scheme write))

  (include "hello/body.scm"))
