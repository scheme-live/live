(import (scheme base) (live list unstable) (live test))

(test-begin "live/list/unstable")

(test-group "list<?"

  (test-eqv #f (list<? < '() '()))
  (test-eqv #f (list<? < '(1) '(1)))
  (test-eqv #f (list<? < '(1 2) '(1 2)))
  (test-eqv #f (list<? < '(1 1 2) '(1 1 2)))

  (test-eqv #t (list<? < '() '(1)))
  (test-eqv #f (list<? < '(1) '()))

  (test-eqv #t (list<? < '(1) '(1 2)))
  (test-eqv #f (list<? < '(1 2) '(1)))

  (test-eqv #t (list<? < '(1 2) '(2)))
  (test-eqv #t (list<? < '(1) '(1 2)))
  (test-eqv #f (list<? < '(2 1) '(2)))

  (test-eqv #t (list<? string<? '("a" "a") '("a" "b")))
  (test-eqv #f (list<? string<? '("a" "b") '("a" "a")))
  (test-eqv #f (list<? string<? '("a" "b") '("a" "a")))

  (test-error (list<? < '(1 2 3 . 4))))

(test-end)
