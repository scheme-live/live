(define-library (live json shim)
  (export directory-list file-regular? arithmetic-shift)
  (import (scheme base)
          (scheme file))

  (cond-expand
   (gambit
    (import (gambit)))
   (gauche
    (import (gauche base)))
   (gerbil
    (import (scheme bitwise)))
   (chibi
    (import (chibi filesystem))
    (import (only (srfi 1) remove)))
   (sagittarius
    (import (sagittarius))
    (import (only (scheme bitwise) arithmetic-shift)))
   (loko
    (import (rename (only (rnrs) bitwise-arithmetic-shift)
                    (bitwise-arithmetic-shift arithmetic-shift))))
   (else))

  (begin

    (cond-expand
     ((or loko sagittarius gerbil)
      (define remove (lambda (p x) x)))
     (else (begin)))


    (cond-expand
     (sagittarius
      (define directory-files read-directory))
     (gauche
      (define arithmetic-shift ash)
      (define file-regular? file-exists?)
      (define (directory-files x)
        (map (lambda (y)
               (substring y (+ (string-length x) 1) (- (string-length y) 1)))
             (glob (string-append x "/*/")))))
     (cyclone
      (define file-regular? file-exists?))
     (chibi (begin))
     (else
      (define file-regular?
        (lambda (x)
          (guard (ex (else #f))
                 (call-with-input-file x
                   (lambda _ #t)))))
      (define directory-files
        (lambda _
          (include "data-index.scm")))))

    (define (directory-list directory)
      (remove (lambda (x) (or (string=? x ".") (string=? x "..")))
              (directory-files directory)))))
