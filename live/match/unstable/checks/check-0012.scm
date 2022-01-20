(import (live unstable))
(import (live match unstable))

(test
	 1
	 (match (list 1 2 1)
		((a b c) (=> fail) (if (equal? a c) a (fail)))
		(_ 'fail)))
