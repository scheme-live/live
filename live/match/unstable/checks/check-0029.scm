#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(define keys
	(lambda (x) (match x (((a b ...) ...) a) (_ 'fail))))

(test 'fail (keys '((a . 1) (b . 2) (c . 3))))
