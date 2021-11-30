(library (live json guile)

  (export directory-list file-regular?)

  (import (guile))

  (begin

    (define (directory-list dirname)
      (define directory (opendir dirname))
      (let loop ((out '()))
        (let ((object (readdir directory)))
          (if (eof-object? object)
              out
              (if (or (string=? object ".") (string=? object ".."))
                  (loop out)
                  (loop (cons object out)))))))

    (define file-regular?  file-exists?)))
