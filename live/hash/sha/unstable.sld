(define-library (live hash sha unstable)
  (export
   sha-1-accumulator
   sha-1-bytevector
   sha-1-port
   sha-256-accumulator
   sha-256-bytevector
   sha-256-port
   sha-512-accumulator
   sha-512-bytevector
   sha-512-port)
  (import (scheme base)
          (live port unstable))
  (cond-expand
    (gauche
     (import (only (gauche base) make)
             (util digest)
             (rfc sha))
     (include "live.gauche.scm"))))
