(define-library (live read)
  (export read-char?
          read-char*)
  (import (scheme base))
  (include "read/live.scm"))
