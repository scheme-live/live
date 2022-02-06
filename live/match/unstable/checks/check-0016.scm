#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(let ()
  (define transpose
    (match-lambda (((a b ...) ...) (cons a (transpose b))) (_ '())))

  (test
	      '((a a) (b b) (c c))
	      (transpose '((a b c) (a b c)))))
