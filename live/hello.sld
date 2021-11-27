(define-library (live hello)
  (export hello)
  (import (scheme base)
          (scheme write))
  (cond-expand
   (chibi
    (include "hello/body.scm"))
   (gambit
    (include "hello/body.scm"))
   (gerbil
    (include "hello/body.scm"))
   (loko
    (include "hello/body.scm"))
   ;; That is the rule picked up by chicken
   (else (include "live/hello/body.scm"))))
