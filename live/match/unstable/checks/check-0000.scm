#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 42 (match 42 (42 42)))
