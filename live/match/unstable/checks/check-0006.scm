(import (live unstable))
(import (live match unstable))


(test 'fail
	    (match (list 1 2 3) (`(a ,b c) b) (_ 'fail)))
