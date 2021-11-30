(import (scheme base)
        (live test)
        (live hash adler32 unstable))

(test-begin "live/hash/adler32")

(test-group "adler32"
  (define lots-of-bytes (make-bytevector 80333 4))
  (test-eqv 1 (adler32-bytevector (bytevector)))
  (test-eqv 851975 (adler32-bytevector (bytevector 1 2 3)))
  (test-eqv 2686183281 (adler32-bytevector lots-of-bytes))
  (test-eqv 2686183281 (call-with-port (open-input-bytevector lots-of-bytes)
                                       adler32-port)))

(test-end)
