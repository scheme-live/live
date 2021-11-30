(define-library (live vector unstable)
  (export vector-cons
          vector-cons-right
          vector-first
          vector-last
          vector-last-index)
  (import (scheme base)
          (live fixnum unstable))
  (include "live.scm"))
