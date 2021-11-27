(define-library (live hello)
  (import (scheme base)
          (scheme write))
  (export hello)
  (cond-expand
   ((or chibi gambit gerbil loko)
    (include "hello/body.scm"))
   ;; That is the rule picked up by chicken
   (else (include "live/hello/body.scm"))))
