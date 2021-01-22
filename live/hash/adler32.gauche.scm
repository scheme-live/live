(define (adler32-bytevector bytes)
  ;; Gauche's adler32 also takes strings, not only bytevectors. What
  ;; are the semantics -- does it use predictable character encoding?
  (typecheck-bytevector adler32-bytevector bytes)
  (gauche-adler32 bytes))

(define (adler32-port in)
  (let ((bytes (make-bytevector 4096)))
    (let loop ((sum 1))
      (let ((n (read-bytevector! bytes in)))
        (if (eof-object? n) sum
            (loop (gauche-adler32 (if (< n (bytevector-length bytes))
                                      (bytevector-copy bytes 0 n)
                                      bytes)
                                  sum)))))))
