(define-library (live vector)
  (export vector-cons
          vector-cons-right
          vector-first
          vector-last
          vector-last-index)
  (import (scheme base)
          (live fixnum))
  (include "vector/live.scm"))
