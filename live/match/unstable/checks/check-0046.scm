(import (live unstable))
(import (live match unstable))


(test 42 (match 42 ((not (? odd? x)) x) (_ 'fail)))
