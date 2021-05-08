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

(define (path-filename path)
  (let loop ((i (string-length path)))
    (cond ((zero? i) path)
          ((char=? #\/ (string-ref path (- i 1)))
           (substring path i (string-length path)))
          (else (loop (- i 1))))))

(define (path-suffix path)
  (let ((path (path-filename path)))
    (let ((n (string-length path)))
      (let left ((a 0))
        (and (< a n)
             (if (char=? #\. (string-ref path a))
                 (left (+ a 1))
                 (let right ((b n))
                   (and (< a b)
                        (if (char=? #\. (string-ref path (- b 1)))
                            (substring path b n)
                            (right (- b 1)))))))))))
