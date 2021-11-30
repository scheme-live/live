(library (live json unstable)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (import (live json base)
          (live json guile))

  (include "body.scm"))
