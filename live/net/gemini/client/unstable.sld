(define-library (live net gemini client unstable)
  (export gemini-get)
  (import (scheme base)
          (live net gemini unstable))
  (cond-expand
    (chicken
     (import (chicken condition) (openssl) (uri-generic))))
  (include "live.scm"))
