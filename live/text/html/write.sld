(define-library (live text html write)
  (export write-html
          write-xml)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write))
  (include "write/live.scm"))
