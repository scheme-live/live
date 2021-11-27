(define-library (live net gemini unstable)
  (export gemini-error?
          gemini-error-response
          make-gemini-response
          gemini-symbol->code
          gemini-code->symbol
          gemini-response?
          gemini-response-code
          gemini-response-first-digit
          gemini-response-second-digit
          gemini-response-success?
          gemini-response-redirect?
          gemini-response-meta
          gemini-response-port
          gemini-response-read-bytevector-all
          gemini-response-read-string-all
          gemini-response-raise
          read-cr-lf-terminated-line)
  (import (scheme base))
  (cond-expand
    (chicken
     (import (chicken condition) (openssl) (uri-generic))))
  (cond-expand
    (chicken

     (define gemini-error?
       (condition-predicate 'gemini-error))

     (define gemini-error-response
       (condition-property-accessor 'gemini-error 'response #f))

     (define (make-gemini-error response)
       (make-property-condition 'gemini-error
                                'message "Gemini request failed"
                                'response response))))
  (include "live.scm"))
