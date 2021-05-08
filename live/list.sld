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
  (include "list/live.scm"))
