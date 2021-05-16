(define (gzip-read binary-port accumulator)
  (define (KiB n) (* 1024 n))
  (define (auto-detect-gzip window-bits) (+ 32 window-bits))
  (call-with-port
   (open-inflating-port binary-port :window-bits (auto-detect-gzip 15))
   (lambda (inflating-port)
     (accumulate-bytevectors-from-port
      accumulator (KiB 512) inflating-port))))
