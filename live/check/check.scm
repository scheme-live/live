(library (live check check)

  (export test-0
          test-1
          test-2
          test-3
          )

  (import (chezscheme) (live check))

  (define test-0
    (check #t #t))

  (define test-1
    (check boolean=? #t #t))

  (define test-2
    (check-raise symbol? (raise 'oops)))

  (define test-3
    (check-values (values 0 1 2) (apply values (list 0 1 2))))


  )
