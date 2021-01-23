(define (accumulate-bytevectors-from-port accumulator buf-size in)
  (let ((buf (make-bytevector buf-size 0)))
    (let loop ()
      (let ((n (read-bytevector! buf in)))
        (if (eof-object? n) (accumulator (eof-object))
            (begin (accumulator (if (= n buf-size) buf
                                    (bytevector-copy buf 0 n)))
                   (loop)))))))
