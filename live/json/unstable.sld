(define-library (live json unstable)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (import (live json base))

  (include "body.scm"))
