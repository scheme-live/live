#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 'fail (match #f ((and x (not #f)) x) (_ 'fail)))
