(library (arew data bbtree-check)
  (export check-000)


  (import (scheme base)
          (scheme list)
          (arew data bbtree)
          (only (chezscheme) random)
          (check))

  (define check-000
    (let* ((integers (delete-duplicates (map (lambda (x) (random (expt 2 16))) (iota 512))))
           (expected (map (lambda (x) (cons x x)) (sort < integers))))
      (check expected (let loop ((integers integers)
                                 (out (make-bbtree <)))
                        (if (null? integers)
                            (bbtree->alist out)
                            (loop (cdr integers)
                                  (bbtree-set out (car integers) (car integers)))))))))
