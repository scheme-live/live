(library (live net http11 unstable)
  (export http-response-read)

  (import (scheme base)
          (scheme cxr)
          (scheme generator))

  (include "body.scm"))
