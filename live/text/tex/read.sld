(define-library (live text tex read)
  (export tex-escape-char
          tex-read-document)
  (import (scheme base)
          (scheme char)
          (live read))
  (include "read/live.scm"))
