(import (live unstable))
(import (live match unstable))


(test '(+ * +)
	    (match '(+ (* (+ 7 2) (/ 5 4)) (sqrt (+ (square x) (square y))))
		    ((a *** 7) a)))
