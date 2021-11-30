(define-library (live hash adler32 unstable)
  (export adler32-accumulator
          adler32-bytevector
          adler32-port)
  (import (scheme base)
          (live port unstable)
          (live typecheck unstable))
  (cond-expand
    (gauche
     (import (rename (only (rfc zlib) adler32)
                     (adler32 gauche-adler32)))
     (include "live.gauche.scm"))))
