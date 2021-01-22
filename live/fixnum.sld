(define-library (live fixnum)
  ;; Re-exported from SRFI 143:
  (export
   fixnum?
   fx*
   fx+
   fx-
   fx-greatest
   fx-least
   fx-width
   fx<=?
   fx<?
   fx=?
   fx>=?
   fx>?
   fxabs
   fxand
   fxarithmetic-shift
   fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxbit-count
   fxbit-field
   fxbit-field-reverse
   fxbit-field-rotate
   fxbit-set?
   fxcopy-bit
   fxeven?
   fxfirst-set-bit
   fxif
   fxior
   fxlength
   fxmax
   fxmin
   fxneg
   fxnegative?
   fxnot
   fxodd?
   fxpositive?
   fxquotient
   fxremainder
   fxsqrt
   fxsquare
   fxxor
   fxzero?)
  ;; Defined in this library:
  (export)
  (import (scheme base))
  (cond-expand (chicken
                (import (chicken fixnum)))
               ((library (srfi 143))
                (import  (srfi 143)))))
