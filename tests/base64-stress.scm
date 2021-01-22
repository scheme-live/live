(import (scheme base) (live encoding filter base64) (live test))

(test-begin "live/encoding/filter/base64-stress")

;; to avoid measuring time in test (doesn't really matter)
(define large-string (make-string 10000001 #\a))
(define large-encoded-string (base64-encode large-string))
(define large-invalid-string (make-string 10000001 #\%))

(test-group
 "decoding"
 (test-equal "decode large string"
       large-string
       (base64-decode large-encoded-string))
 (test-equal "decode large string of invalid chars"
       ""
       (base64-decode large-invalid-string)))

;; Not on a 64-bit machine! :)
;; (test-error "encode string of length 16,000,000 signals an error"
;;             (base64-encode (make-string 16000000)))

(test-end)
