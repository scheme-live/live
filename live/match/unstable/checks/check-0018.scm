#!/usr/bin/env -S scheme-live current run
(import (live unstable))
(import (live match unstable))


(test 'failure (match (list 1 2) ((a b c **1) c) (_ 'failure)))
