(import (live unstable))
(import (live match unstable))


(test 'fail
	    (match (list 'A 'B 'A) (`(,a b ,a) a) (_ 'fail)))
