#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 1 (match 1 ((and x (not #f)) x) (_ 'fail)))
