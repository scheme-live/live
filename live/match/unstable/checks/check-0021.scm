(import (live unstable))
(import (live match unstable))


(test '((a c e) (b d f))
	    (match '((a b) (c d) (e f))
		   (((x y) =.. 3) (list x y))
		   (_ 'fail)))
