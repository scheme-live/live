(define include-types '("fn"))

;;

(define (displayln x) (display x) (newline))

(define (filter f xs)
  (fold (lambda (x acc) (if (f x) (cons x acc) acc)) '() xs))

(define (append-map f xs) (apply append (map f xs)))

(define (generator-fold merge state gen)
  (let ((x (gen)))
    (if (eof-object? x) state (generator-fold merge (merge x state) gen))))

(define (path-append a b) (string-append a "/" b))

(define (split char string)
  (let loop ((a 0) (b 0) (fields '()))
    (cond ((= b (string-length string))
           (reverse (cons (substring string a b) fields)))
          ((char=? char (string-ref string b))
           (loop (+ b 1) (+ b 1) (cons (substring string a b) fields)))
          (else
           (loop a (+ b 1) fields)))))

;;

(define (string-append-map proc str)
  (let loop ((i 0) (acc ""))
    (if (= i (string-length str)) acc
        (loop (+ i 1) (string-append acc (proc (string-ref str i)))))))

(define (nginx-url-encode str)
  (string-append-map (lambda (char)
                       (case char
                         ((#\# #\% #\& #\+ #\?)
                          (string-append "%" (string-upcase
                                              (number->string
                                               (char->integer char) 16))))
                         (else (string char))))
                     str))

(define (nginx-double-quote str)
  (string-append "\""
                 (string-append-map (lambda (char)
                                      (case char
                                        ((#\\ #\") (string #\\ char))
                                        (else (string char))))
                                    str)
                 "\""))

(define (dir->index-file dir) (path-append dir "index.tsv"))

(define (find-indexed-dirs)
  (filter (lambda (dir)
            (and (eq? 'directory (file-type dir))
                 (file-exists? (dir->index-file dir))))
          (directory-files)))

(define (tsv-line->nginx-lines line dir)
  (let* ((fields (split #\tab line))
         (path (list-ref fields 0))
         (type (list-ref fields 1))
         (name (list-ref fields 2)))
    (if (not (member type include-types)) '()
        (list ""
              (nginx-double-quote (nginx-url-encode (path-append dir name)))
              (string-append (nginx-double-quote (path-append dir path))
                             ";")))))

(define (dir->nginx dir)
  (with-input-from-file (dir->index-file dir)
    (lambda ()
      (generator-fold
       (lambda (line lines) (append lines (tsv-line->nginx-lines line dir)))
       '() read-line))))

(for-each displayln (append-map dir->nginx (find-indexed-dirs)))
