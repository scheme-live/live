#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test #t (match #f ((and) #t) (_ #f)))
