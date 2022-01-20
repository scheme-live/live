(import (live unstable))
(import (live match unstable))


(test 'fail
	    (match '((a b) (c d) (e f) (g h))
		    (((x y) =.. 3) (list x y))
		    (_ 'fail)))
