(define-library (live typecheck)
  (export typecheck-bytevector
          typecheck-string)
  (import (scheme base))
  (cond-expand
    (chicken (include "typecheck.chicken.scm"))
    (else    (include "typecheck.r7rs.scm"))))
