(define-library (live encoding filter gzip)
  (export gzip-read)
  (import (scheme base) (live port))
  (cond-expand
    (gauche
     (import (gauche base) (rfc zlib))
     (include "gzip.gauche.scm"))))
