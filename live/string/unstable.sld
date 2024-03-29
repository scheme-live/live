(define-library (live string unstable)
  ;; Re-exported from SRFI 13:
  (export
   string-concatenate-reverse
   string-every
   string-index
   string-index-right
   string-join
   string-null?
   string-prefix?
   string-suffix?
   string-trim
   string-trim-both
   string-trim-right)
  ;; Defined in this library:
  (export
   string-blank?
   string-char-prefix?
   string-char-suffix?
   string-last-index
   string-split
   with-input-from-string
   with-output-to-string)
  (import (scheme base)
          (scheme char))
  (cond-expand
   ;; ((library (srfi 130))
   ;; (import  (srfi 130)))
   (chicken
    (import  (srfi 13)))
   (else
    (include "srfi-13.scm")))
  (include "live.scm"))
