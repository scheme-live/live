(import (live string) (live test))

(test-begin "live/string")

(test-group "string-last-index"
  (test-eqv #f (string-last-index ""))
  (test-eqv 0 (string-last-index "1"))
  (test-eqv 1 (string-last-index "12"))
  (test-eqv 2 (string-last-index "123")))

(test-group "string-char-prefix?"
  (test-eqv #f (string-char-prefix? #\x ""))
  (test-eqv #f (string-char-prefix? #\x "X"))
  (test-eqv #t (string-char-prefix? #\x "x"))
  (test-eqv #t (string-char-prefix? #\x "xy")))

(test-group "string-char-suffix?"
  (test-eqv #f (string-char-suffix? #\y ""))
  (test-eqv #f (string-char-suffix? #\y "Y"))
  (test-eqv #t (string-char-suffix? #\y "y"))
  (test-eqv #t (string-char-suffix? #\y "xy")))

(test-group "string-blank?"
  (test-eqv #t (string-blank? ""))
  (test-eqv #t (string-blank? " "))
  (test-eqv #f (string-blank? "a"))
  (test-eqv #f (string-blank? "a "))
  (test-eqv #f (string-blank? " a"))
  (test-eqv #f (string-blank? " a ")))

(test-end)
