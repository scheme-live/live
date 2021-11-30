(import (scheme base)
        (live typecheck unstable)
        (live test))

(test-begin "live/typecheck")

(test-group "typecheck-bytevector"
  (let ((foo (bytevector)))
    (test-assert (begin (typecheck-bytevector just-testing foo)
                        #t)))
  (let ((foo 123))
    (test-error (typecheck-bytevector just-testing foo))))

(test-group "typecheck-string"
  (let ((foo (string)))
    (test-assert (begin (typecheck-string just-testing foo)
                        #t)))
  (let ((foo 123))
    (test-error (typecheck-string just-testing foo))))

(test-end)
