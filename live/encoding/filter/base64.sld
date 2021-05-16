(define-library (live encoding filter base64)
  (export base64-decode
          base64-encode
          base64-line-breaks)
  (import (scheme base)
          (scheme case-lambda)
          (live bitwise)
          (live fixnum)
          (live typecheck)
          (only (live string) string-concatenate-reverse))
  (cond-expand (chicken
                (import (only (chicken io) read-string!)))
               (else
                ;; TODO: Temporary hack. Not efficient. In any case,
                ;; it's suspicious that we're using a `read-string!`
                ;; at all. Characters don't take a constant number of
                ;; bytes so it's fundamentally inefficient. We should
                ;; probably be using `read-bytevector!` instead.
                (begin
                  (define (read-string! buflen buf in)
                    (let ((str (read-string buflen in)))
                      (if (eof-object? str) 0
                          (let ((n (string-length str)))
                            (string-copy! buf 0 str 0 n)
                            n)))))))
  (include "base64/live.scm"))
