(import (live unstable))
(import (live match unstable))


(test #t (match 1 ((not 2) #t)))
