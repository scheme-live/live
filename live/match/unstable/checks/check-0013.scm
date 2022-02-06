#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test #t
	    (match (list 1 2) ((1 2 3 ...) #t)))
