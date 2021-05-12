(define-library (live list)
  (export
   ;; Re-exported from SRFI 1:
   any
   drop-while
   every
   first
   fold
   last
   last-pair
   unfold
   ;; Defined in this library:
   circular-list?
   cons-right
   dotted-list?
   last-index
   length-tail
   map/odd
   proper-list?)
  (import (scheme base) (srfi 1) (live fixnum))
  (include "list/live.scm"))
