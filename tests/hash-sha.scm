(import (scheme base)
        (live test)
        (live hash sha unstable))

(test-begin "live/hash/sha")

(test-group "sha-1"
  (test-equal "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (sha-1-bytevector (bytevector)))
  (test-equal "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"
    (sha-1-bytevector (string->utf8 "hello world"))))

(test-group "sha-256"
  (test-equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (sha-256-bytevector (bytevector)))
  (test-equal "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
    (sha-256-bytevector (string->utf8 "hello world"))))

(test-group "sha-512"
  (test-equal "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
    (sha-512-bytevector (bytevector)))
  (test-equal "309ecc489c12d6eb4cc40f50c902f2b4d0ed77ee511a7c7a9bcd3ca86d4cd86f989dd35bc5ff499670da34255b45b0cfd830e81f605dcf7dc5542e93ae9cd76f"
    (sha-512-bytevector (string->utf8 "hello world"))))

(test-end)
