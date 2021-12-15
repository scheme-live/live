(library (live net http11 unstable)
  (export
   http-error
   http-error-message
   http-error-irritants
   http-request-read
   http-request-read*
   http-request-write
   http-request-write*
   http-response-read
   http-response-read*
   http-response-write
   http-response-write*)

  (import (scheme base)
          (scheme cxr)
          (scheme generator))

  (include "body.scm"))
