(define-library (live hello)
  (import (scheme base)
          (scheme write))
  (export hello)

  (begin
    (cond-expand
     ((or chicken sagittarius guile gambit gerbil loko gauche)
      (include "hello/body.scm"))
     ((or chibi mit cyclone)
      (include "live/hello/body.scm"))
     (else))))
