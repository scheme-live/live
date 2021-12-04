(define-library (live hello)
  (import (scheme base)
          (scheme write))
  (export hello)

  (begin
    (cond-expand
     ((or chicken sagittarius guile mit gambit gerbil loko gauche)
      (include "hello/body.scm"))
     (chibi
      (include "live/hello/body.scm"))
     (else))))
