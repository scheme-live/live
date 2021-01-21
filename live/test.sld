(define-library (live test)
  ;; Re-exported from SRFI 64:
  ;; TODO: Should we export more stuff from SRFI 64?
  (export
   test-group
   test-assert
   test-eq
   test-eqv
   test-equal
   test-error
   test-read-eval-string)
  ;; TODO: These are controversial. Many schemers think we should use
  ;; (test-group ...) instead, so that all the test cases in a group
  ;; are nested inside the same (test-group ...) form.
  (export
   test-begin
   test-end)
  (import (scheme base) (srfi 64)))
