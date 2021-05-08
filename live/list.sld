(define-library (live list)
  (export
   ;; Re-exported from SRFI 1:
   any
   drop-while
   every
   fold
   last
   last-pair
   unfold
   ;; Defined in this library:
   circular-list?
   dotted-list?
   length-tail
   map/odd
   proper-list?)
  (import (scheme base) (srfi 1))
  (include "list/live.scm"))
