(define-library (live json unstable)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (cond-expand
   ((or cyclone mit)
    (import (scheme base)))
   (else))

  (import (live unstable))

  (include "body.scm"))
