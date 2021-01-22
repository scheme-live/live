(define (digest-accumulator digest)
  (lambda (bytevector)
    (if (eof-object? bytevector)
        (digest-hexify (digest-final! digest))
        (begin (digest-update! digest bytevector) #f))))

(define (digest-bytevector digest bytevector)
  (let ((acc (digest-accumulator digest)))
    (acc bytevector)
    (acc (eof-object))))

(define (digest-port digest in)
  (let ((acc (digest-accumulator digest))
        (buf (make-bytevector 4096)))
    (let loop ()
      (let ((n (read-bytevector! buf in)))
        (if (eof-object? n)
            (acc (eof-object))
            (begin (acc (if (< n (bytevector-length buf))
                            (bytevector-copy buf 0 n)
                            buf))
                   (loop)))))))

;;

(define (sha-1-accumulator)
  (digest-accumulator (make <sha1>)))

(define (sha-256-accumulator)
  (digest-accumulator (make <sha256>)))

(define (sha-512-accumulator)
  (digest-accumulator (make <sha512>)))

;;

(define (sha-1-bytevector bytevector)
  (digest-bytevector (make <sha1>) bytevector))

(define (sha-256-bytevector bytevector)
  (digest-bytevector (make <sha256>) bytevector))

(define (sha-512-bytevector bytevector)
  (digest-bytevector (make <sha512>) bytevector))

;;

(define (sha-1-port in)
  (digest-port (make <sha1>) in))

(define (sha-256-port in)
  (digest-port (make <sha256>) in))

(define (sha-512-port in)
  (digest-port (make <sha512>) in))
