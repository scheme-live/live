(import (live unstable))
(import (live match unstable))


(test '(3 3 3) (match (list 1 2 3 3 3) ((a b c **1) c)))
