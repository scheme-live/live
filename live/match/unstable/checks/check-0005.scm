#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 2 (match (list 1 2 3) ((_ b _2) b)))
