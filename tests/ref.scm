(import (scheme base) (live ref) (live test))

(test-begin "live/ref")

(test-group "ref"
  (define people '(("Mick" (dogs . #(((breed . "Dalmatian")))))))
  (test-equal "Dalmatian" (ref people "Mick" 'dogs 0 'breed)))

(test-end)
