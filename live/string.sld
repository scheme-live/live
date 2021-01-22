(define-library (live string)
  ;; Re-exported from SRFI 13:
  (export
   string-concatenate-reverse
   string-every
   string-join
   string-null?
   string-prefix?
   string-suffix?)
  ;; Defined in this library:
  (export
   string-blank?
   string-char-prefix?
   string-char-suffix?
   string-last-index)
  (import (scheme base)
          (scheme char))
  (cond-expand ((library (srfi 130))
                (import  (srfi 130)))
               ((library (srfi 13))
                (import  (srfi 13))))
  (include "string.scm"))
