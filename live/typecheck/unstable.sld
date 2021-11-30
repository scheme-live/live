(define-library (live typecheck unstable)
  (export typecheck-bytevector
          typecheck-string)
  (import (scheme base))
  (cond-expand
    (chicken
     (include "live.chicken.scm"))
    (else
     (include "live.r7rs.scm"))))
