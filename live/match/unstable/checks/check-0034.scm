#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))

(test #t (match 1 ((and) #t)))
