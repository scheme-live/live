(define-library (live bitwise)
  ;; Re-exported from SRFI 151:
  (export
   any-bit-set?
   arithmetic-shift
   bit-count
   bit-set?
   bitwise-and
   bitwise-ior
   bitwise-not
   bitwise-xor
   copy-bit
   every-bit-set?
   first-set-bit)
  ;; Defined in this library:
  (export)
  (import (scheme base))
  (cond-expand (chicken
                (import (chicken bitwise)))
               ((library (srfi 151))
                (import  (srfi 151)))))
