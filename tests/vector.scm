(import (scheme base) (live vector unstable) (live test))

(test-begin "live/vector/unstable")

(test-group "vector<?"

  (test-eqv #f (vector<? < '#() '#()))
  (test-eqv #f (vector<? < '#(1) '#(1)))
  (test-eqv #f (vector<? < '#(1 2) '#(1 2)))
  (test-eqv #f (vector<? < '#(1 1 2) '#(1 1 2)))

  (test-eqv #t (vector<? < '#() '#(1)))
  (test-eqv #f (vector<? < '#(1) '#()))

  (test-eqv #t (vector<? < '#(1) '#(1 2)))
  (test-eqv #f (vector<? < '#(1 2) '#(1)))

  (test-eqv #t (vector<? < '#(1 2) '#(2)))
  (test-eqv #t (vector<? < '#(1) '#(1 2)))
  (test-eqv #f (vector<? < '#(2 1) '#(2)))

  (test-eqv #t (vector<? string<? '#("a" "a") '#("a" "b")))
  (test-eqv #f (vector<? string<? '#("a" "b") '#("a" "a")))
  (test-eqv #f (vector<? string<? '#("a" "b") '#("a" "a"))))

(test-end)
