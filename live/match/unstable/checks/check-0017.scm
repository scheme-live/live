#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(define first-column
  (lambda (e)
    (match e (((a b ...) ...) a))))

(test '(a a a)
	    (first-column '((a b c) (a b c) (a b c))))
