(define-library (live path)
  (export path-absolute-posix?
          path-append
          path-concatenate)
  (import (scheme base) (live list) (live string))
  (include "path.scm"))
