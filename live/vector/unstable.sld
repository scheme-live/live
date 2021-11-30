(define-library (live vector unstable)
  (export vector-cons
          vector-cons-right
          vector-first
          vector-last
          vector-last-index
          vector<?)
  (import (scheme base)
          (live fixnum unstable))
  (include "live.scm"))
