(define-library (live typecheck)
  (export typecheck-string)
  (import (scheme base))
  (cond-expand
    (chicken
     (include "typecheck.chicken.scm"))
    (else
     (begin
       (define-syntax typecheck-string
         (syntax-rules ()
           ((_ who var)
            #f)))))))
