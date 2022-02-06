#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))

(test 'ok (match '#(1) ('#(1) 'ok)))
