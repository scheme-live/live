(define-library (live json unstable)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (cond-expand
   (mit
    (import (scheme base))))
  (import (live json base))

  (cond-expand
   (mit
    (import (scheme base))))

  (include "body.scm"))
