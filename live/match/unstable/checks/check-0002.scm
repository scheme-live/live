(import (live unstable))
(import (live match unstable))


(test 'ok
	     (let ((lts (list 'a "b" #f 2 '() #\c)))
	       (match lts (('a "b" #f 2 '() #\c) 'ok))))
