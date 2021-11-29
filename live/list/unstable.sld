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
   list<?
   map/odd
   proper-list?)
  (import (scheme base)

          ;; TODO: Should we prefer the SRFI 1 copies of these
          ;; procedures?
          (except (srfi 1)
                  circular-list?
                  dotted-list?
                  proper-list?)

          (live fixnum unstable))
  (include "live.scm"))
