(define-library (live list unstable)
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
  (import (scheme base)

          ;; TODO
          (except (srfi 1)
                  circular-list?
                  dotted-list?
                  proper-list?)

          (live fixnum unstable))
  (include "live.scm"))
