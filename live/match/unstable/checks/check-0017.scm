(import (live unstable))
(import (live match unstable))


(define first-column
  (match-lambda (((a _ ...) ...) a)))

(test '(a a a)
	    (first-column '((a b c) (a b c) (a b c)))))
