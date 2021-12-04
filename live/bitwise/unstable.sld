(define-library (live bitwise unstable)
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
  (cond-expand
   (chicken
    (import (srfi 151)))
   (else)))

;; TODO: (chicken bitwise)
