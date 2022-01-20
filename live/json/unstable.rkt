#!r7rs
(define-library (live json unstable)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (import (live unstable))

  (include "body.scm"))
