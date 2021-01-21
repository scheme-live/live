(import test)
(import base64
        (chicken string))

(define lorem-ipsum "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
(define lorem-ipsum-base64
  '("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQs"
    "IHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFn"
    "bmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRh"
    "dGlvbiB1bGxhbWNvIGxhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2Vx"
    "dWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUg"
    "dmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0"
    "ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBx"
    "dWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4="
    ""))   ; trailing empty for intersperse

(test-group
 "encoding"
 (test "encode string of length 0"
       ""
       (base64-encode ""))
 (test "encode string of length 1"
       "YQ=="
       (base64-encode "a"))
 (test "encode string of length 2"
       "YWI="
       (base64-encode "ab"))
 (test "encode string of length 3"
       "YWJj"
       (base64-encode "abc"))
 (test "encode string of length 5*3"
       "YWJjZGVmZ2hpamtsbW5v"
       (base64-encode "abcdefghijklmno"))
 (test "encode string of length 5*3+1"
       "YWJjZGVmZ2hpamtsbW5vcA=="
       (base64-encode "abcdefghijklmnop"))
 (test "encode string of length 5*3+2"
       "YWJjZGVmZ2hpamtsbW5vcHE="
       (base64-encode "abcdefghijklmnopq"))
 (test "encode string of length 6*3"
       "YWJjZGVmZ2hpamtsbW5vcHFy"
       (base64-encode "abcdefghijklmnopqr"))
 (test "encode binary string"
       "3q2+78r+sAs="
       (base64-encode "\xde\xad\xbe\xef\xca\xfe\xb0\x0b"))
 (test "lorem ipsum"
       (apply string-append lorem-ipsum-base64)
       (base64-encode lorem-ipsum))
 (let ((s (make-string (+ 10 (* 57 60)) #\Q)))  ; past one input buffer
   (test "port > 1 buffer length -> port"
         (base64-encode s)
         (get-output-string (base64-encode (open-input-string s)
                                           (open-output-string))))
   (test "port > 1 buffer length -> string"
         (base64-encode s)
         (base64-encode (open-input-string s)))))

(test-group
 "encoding linebreaks"
 (parameterize ((base64-line-breaks #t))
   (test "encode empty string"
         ""
         (base64-encode ""))
   (test "encode 9 chars"
         "YWFhYWFhYWFh\r\n"
         (base64-encode (make-string 9 #\a)))
   (test "encode 55 chars"
         "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYQ==\r\n"
         (base64-encode (make-string 55 #\a)))
   (test "encode 56 chars"
         "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWE=\r\n"
         (base64-encode (make-string 56 #\a)))
   (test "encode 57 chars"
         "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh\r\n"
         (base64-encode (make-string 57 #\a)))
   (test "encode 58 chars"
         "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh\r\nYQ==\r\n"
         (base64-encode (make-string 58 #\a)))
   (test "encode 57*2 chars"
         (string-append "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh"
                        "\r\n"
                        "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh"
                        "\r\n")
         (base64-encode (make-string (* 57 2) #\a)))
   (test "encode 57*2+1 chars"
         (string-append "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh"
                        "\r\n"
                        "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFh"
                        "\r\n"
                        "YQ=="
                        "\r\n")
         (base64-encode (make-string (+ 1 (* 57 2)) #\a)))
   (let ((lorem-ipsum-encoded (string-intersperse lorem-ipsum-base64 "\r\n")))
     (test "lorem ipsum"
           lorem-ipsum-encoded
           (base64-encode lorem-ipsum))
     (test "lorem ipsum string -> port"
           lorem-ipsum-encoded
           (get-output-string (base64-encode lorem-ipsum (open-output-string))))
     (test "lorem ipsum port -> string"
           lorem-ipsum-encoded
           (base64-encode (open-input-string lorem-ipsum)))
     (test "lorem ipsum port -> port"
           lorem-ipsum-encoded
           (get-output-string (base64-encode (open-input-string lorem-ipsum)
                                             (open-output-string)))))
   ))

;; to avoid measuring time in test (doesn't really matter)
(define large-string (make-string 10000001 #\a))
(define large-encoded-string (base64-encode large-string))
(define large-invalid-string (make-string 10000001 #\%))

(test-group
 "decoding"
 (test "decode empty string -> empty"
       ""
       (base64-decode ""))
 (test "decode string Y -> empty"
       ""
       (base64-decode "Y"))
 (test "decode string YW -> a"
       "a"
       (base64-decode "YW"))
 (test "decode string YW= -> a"
       "a"
       (base64-decode "YW="))
 (test "decode string YW== -> a"
       "a"
       (base64-decode "YW=="))
 (test "decode string YWJ => ab"
       "ab"
       (base64-decode "YWJ"))
 (test "decode string YWJ= -> ab"
       "ab"
       (base64-decode "YWJ="))
 (test "decode string YWJj -> abc"
       "abc"
       (base64-decode "YWJj"))
 (test "decode string YW%J^jZ -> abc"
       "abc"
       (base64-decode "YW%J^jZ"))
 (test "decode skips invalid chars"
       "abcdefghijklmnop"
       (base64-decode "YWJjZG(VmZ#2hp@amtsb%&W5v**cA======"))
 (test "decode binary string"
       "\xde\xad\xbe\xef\xca\xfe\xb0\x0b"
       (base64-decode "3q2+78r+sAs="))
 (test "decode large string"
       large-string
       (base64-decode large-encoded-string))
 (test "decode large string of invalid chars"
       ""
       (base64-decode large-invalid-string))
 (test "decode lorem ipsum with linebreaks"
       lorem-ipsum
       (base64-decode (string-intersperse lorem-ipsum-base64 "\r\n"))))

;; Not on a 64-bit machine! :)
;; (test-error "encode string of length 16,000,000 signals an error"
;;             (base64-encode (make-string 16000000)))

(test-exit)
