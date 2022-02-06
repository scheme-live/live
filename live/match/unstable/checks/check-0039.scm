#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test #f (match 1 ((or) #t) (else #f)))
