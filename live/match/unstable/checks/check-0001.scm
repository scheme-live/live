#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test #t (let ((ls (list 1 2 3)))
	   (match ls
	     ((1 2 3) #t))))
