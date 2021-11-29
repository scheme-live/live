(import (scheme base) (live private helpers) (live test))

(test-begin "live/private/helpers")

(test-group "unwind-protect"

  (test-equal '(1)
    (let ((nums '()))
      (unwind-protect
        (lambda ()
          (set! nums (append nums '(1)))
          nums)
        (lambda ()
          (set! nums (append nums '(2)))))))

  (test-equal '(1 2)
    (let ((nums '()))
      (unwind-protect
        (lambda () (set! nums (append nums '(1))))
        (lambda () (set! nums (append nums '(2)))))
      nums))


  (test-equal '((1) (1 2))
    (let* ((nums '())
           (a (unwind-protect
                (lambda ()
                  (set! nums (append nums '(1)))
                  nums)
                (lambda ()
                  (set! nums (append nums '(2))))))
           (b nums))
      (list a b)))

  (test-equal '(0 0 1 1 2 2 3 3 4 4 5 5)
    (let ((nums '()) (n 5))
      (let ((resume (call/cc
                     (lambda (announce)
                       (dynamic-wind
                         (lambda () #f)
                         (lambda ()
                           (let loop ()
                             (if (zero? n)
                                 (lambda () nums)
                                 (begin (call/cc announce)
                                        (set! n (- n 1))
                                        (loop)))))
                         (lambda ()
                           (call/cc announce)))))))
        (set! nums (cons n nums))
        (resume))))

  (test-equal '(0 1 2 3 4 5 5)
    (let ((nums '()) (n 5))
      (let ((resume (call/cc
                     (lambda (announce)
                       (unwind-protect
                         (lambda ()
                           (let loop ()
                             (if (zero? n)
                                 (lambda () nums)
                                 (begin (call/cc announce)
                                        (set! n (- n 1))
                                        (loop)))))
                         (lambda ()
                           (call/cc announce)))))))
        (set! nums (cons n nums))
        (resume)))))

(test-end)
