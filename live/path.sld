(define-library (live path)
  (export path-absolute-posix?
          path-append
          path-concatenate
          path-filename
          path-suffix)
  (import (scheme base) (live list) (live string))
  (include "path.scm"))
