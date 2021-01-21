;; TODO: Review John's pathname proposal and discuss with him.
;; https://github.com/johnwcowan/r7rs-work/blob/master/PathnamesPython.md

(define (path-absolute-posix? path)
  (string-char-prefix? #\/ path))

(define (path-concatenate parts)
  ;; Behavior modeled after Python's `os.path.join`.
  (fold (lambda (next-part path-so-far)
          (cond ((path-absolute-posix? next-part)
                 next-part)
                ((or (string-char-suffix? #\/ path-so-far)
                     (string-null? path-so-far))
                 (string-append path-so-far next-part))
                (else
                 (string-append path-so-far "/" next-part))))
        "" parts))

(define (path-append . parts)
  (path-concatenate parts))
