(import (live unstable))
(import (live match unstable))

(test 'fail
	    (match '((a b) (a b) (a b) (a b) (a b))
		    (((x y) *.. 2 4) (list x y))
		    (_ 'fail)))
