(import (live unstable))
(import (live match unstable))


(test 2 (match (list 1 2 3) ((a b c) b)))
