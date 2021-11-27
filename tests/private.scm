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
      (list a b))))

(test-end)
