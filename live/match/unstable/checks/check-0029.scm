(import (live unstable))
(import (live match unstable))


(define keys
	(match-lambda (((a _ ...) ...) a) (_ 'fail)))

(test 'fail (keys '((a . 1) (b . 2) (c . 3)))))
