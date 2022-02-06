#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 'fail (match 42 ((? odd? x) x) (_ 'fail)))
