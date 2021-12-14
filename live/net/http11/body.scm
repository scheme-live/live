(define-record-type <http-error>
  (make-http-error message payload)
  http-error?
  (message http-error-message)
  (payload http-error-payload))

(define (generator-until generator predicate?)
  (lambda ()
    (let ((item (generator)))
      (if (predicate? item)
          (eof-object)
          item))))

(define (space? byte)
  (= byte (char->integer #\space)))

(define (two-dots? byte)
  (= byte (char->integer #\:)))

(define (expected-read expected continuation)
  (lambda (char)
    (if (char=? char expected)
        (values #t continuation)
        (raise (make-http-error "Expected another character" (cons expected char))))))

(define (http-request-line-read generator)
  (define method '())
  (define uri '())
  (define version '())

  (define (method-read char)
    (if (char=? char #\space)
        (values #t uri-read)
        (begin
          (set! method (cons char method))
          (values #t method-read))))

  (define (uri-read char)
    (if (char=? char #\space)
        (values #t start-version-read)
        (begin
          (set! uri (cons char uri))
          (values #t uri-read))))

  (define (major-version-read char)
    (let ((major (string->number (list->string (list char)))))
      (if major
          (begin
            (set! version (cons major version))
            (values #t (expected-read #\. minor-version-read)))
          (raise (make-http-error "Invalid major version" char)))))

  (define (minor-version-read char)
    (let ((minor (string->number (list->string (list char)))))
      (if minor
          (begin
            (set! version (cons minor version))
            (values #t return-read))
          (raise (make-http-error "Invalid minor version" char)))))

  (define (newline-read char)
    (if (char=? char #\newline)
        (values #f #f)
        (raise (make-http-error "Invalid end of line" char))))

  (define start-version-read
    ;; TODO: make it nice
    (expected-read #\H
                   (expected-read #\T
                                  (expected-read #\T
                                                 (expected-read #\P
                                                                (expected-read #\/
                                                                               major-version-read))))))

  (define return-read (expected-read #\return newline-read))

  (let loop ((k method-read)
             (char (generator)))

    (when (eof-object? char)
      (raise (make-http-error "Request line truncated!" #f)))

    (call-with-values (lambda () (k (integer->char char)))
      (lambda (continue? continuation)
        (if continue?
            (loop continuation (generator))
            (values (list->string (reverse method))
                    (list->string (reverse uri))
                    (apply cons (reverse version))))))))

(define (put-string* accumulator string)
  (accumulator (string->utf8 string)))

(define (http-request-line-write accumulator method uri version)
  (put-string* accumulator
              (string-append method
                             " "
                             uri
                             " HTTP/"
                             (number->string (car version))
                             "."
                             (number->string (cdr version))))
  ;; eol
  (put-string* accumulator "\r\n"))

(define (http-response-line-read generator)
  (define version '())
  (define status-code '())
  (define reason '())

  (define (major-version-read char)
    (let ((major (string->number (list->string (list char)))))
      (if major
          (begin
            (set! version (cons major version))
            (values #t (expected-read #\. minor-version-read)))
          (raise (make-http-error "Invalid major version" char)))))

  (define (minor-version-read char)
    (let ((minor (string->number (list->string (list char)))))
      (if minor
          (begin
            (set! version (cons minor version))
            (values #t (expected-read #\space status-code-read-one)))
          (raise (make-http-error "Invalid minor version" char)))))

  (define (status-code-read-one char)
    (let ((digit (string->number (list->string (list char)))))
      (if digit
          (begin
            (set! status-code (cons char status-code))
            (values #t status-code-read-two))
          (raise (make-http-error "Invalid http status code digit" char)))))

  (define start-version-read
    ;; TODO: make it nice
    (expected-read #\H
                   (expected-read #\T
                                  (expected-read #\T
                                                 (expected-read #\P
                                                                (expected-read #\/
                                                                               major-version-read))))))

  (define (status-code-read-two char)
    (let ((digit (string->number (list->string (list char)))))
      (if digit
          (begin
            (set! status-code (cons char status-code))
            (values #t status-code-read-three))
          (raise (make-http-error "Invalid http status code digit" char)))))

  (define (status-code-read-three char)
    (let ((digit (string->number (list->string (list char)))))
      (if digit
          (begin
            (set! status-code (cons char status-code))
            (values #t (expected-read #\space reason-read)))
          (raise (make-http-error "Invalid http status code digit" char)))))

  (define (reason-read char)
    (if (char=? char #\return)
        (values #t newline-read)
        (begin
          (set! reason (cons char reason))
          (values #t reason-read))))

  (define (newline-read char)
    (if (char=? char #\newline)
        (values #f #f)
        (raise (make-http-error "Invalid end of line" char))))

  (let loop ((k start-version-read)
             (char (generator)))
    (when (eof-object? char)
      (raise (make-http-error "Response line truncated!" #f)))
    (call-with-values (lambda () (k (integer->char char)))
      (lambda (continue? continuation)
        (if continue?
            (loop continuation (generator))
            (values (apply cons (reverse version))
                    (string->number (list->string (reverse status-code)))
                    (list->string (reverse reason))))))))

(define (http-response-line-write accumulator version code reason)
  ;; version
  (put-string* accumulator "HTTP/")
  (put-string* accumulator
              (string-append (number->string (car version))
                             "."
                             (number->string (cdr version))))
  ;; code
  (put-string* accumulator (string-append " " (number->string code)))
  ;; reason
  (put-string* accumulator (string-append " " reason))
  ;; eol
  (put-string* accumulator "\r\n"))

(define (http-headers-read generator)

  (define (http-header-read generator)

    (define key '())
    (define value '())

    (define (key-read char)
      (case char
        ((#\return) (values #t newline-read))
        ((#\newline) (raise (make-http-error "Unexpected newline characters inside header line" #f)))
        ((#\:) (values #t value-read))
        (else (set! key (cons char key))
              (values #t key-read))))

    (define (value-read char)
      (case char
        ((#\return) (values #t newline-read))
        ((#\newline) (raise (make-http-error "Unexpected newline characters inside header line" #f)))
        ((#\space) (if (null? value)
                       (values #t value-read)
                       (begin (set! value (cons char value))
                              (values #t value-read))))
        (else (set! value (cons char value))
              (values #t value-read))))

    (define (newline-read char)
      (if (char=? char #\newline)
          (values #f #f)
          (raise (make-http-error "Invalid end of line" char))))

    (let loop ((k key-read)
               (char (generator)))
      (when (eof-object? char)
        (raise (make-http-error "Header line truncated!" #f)))
      (call-with-values (lambda () (k (integer->char char)))
        (lambda (continue? continuation)
          (if continue?
              (loop continuation (generator))
              (if (null? key)
                  '()
                  (cons (list->string (reverse key))
                        (string-trim-right (list->string (reverse value))))))))))

  (let loop ((out '())
             (header (http-header-read generator)))
    (if (null? header)
        out
        (loop (cons header out) (http-header-read generator)))))

(define (http-headers-write accumulator headers)
  (let loop ((headers headers))
    (if (null? headers)
        (put-string* accumulator "\r\n")
        (let ((header (car headers)))
          (put-string* accumulator (car header))
          (put-string* accumulator ": ")
          (put-string* accumulator (cdr header))
          (put-string* accumulator "\r\n")
          (loop (cdr headers))))))

(define (http-chunks-generator generator)

  (define chunk-length '())
  (define chunk-extension '())

  (define (chunk-length-read char)
    (case char
      ((#\return) (values #t newline-read))
      ((#\newline) (make-http-error "Invalid newline character in chunkline" #\newline))
      ((#\;) (values #t chunk-extension-read))
      (else (set! chunk-length (cons char chunk-length))
            (values #t chunk-length-read))))

  (define (chunk-extension-read char)
    (case char
      ((#\return) (values #t newline-read))
      ((#\newline) (make-http-error "Invalid newline character in chunkline" #\newline))
      (else
       (set! chunk-extension (cons char chunk-extension))
       (values #t chunk-extension-read))))

  (define (newline-read char)
    (if (char=? char #\newline)
        (values #f #f)
        (raise (make-http-error "Invalid end of line" char))))

  (define (chunk-generator generator chunk-length)
    (let ((length (string->number (list->string (reverse chunk-length)) 16)))
      (if (zero? length)
          eof-object
          (generator/continuation
           (gtake generator length)
           (lambda () (generator) (generator) (eof-object))))))

  (lambda ()
    (set! chunk-length '())
    (set! chunk-extension '())
    (let loop ((k chunk-length-read)
               (char (generator)))
      (when (eof-object? char)
        (raise (make-http-error "Some chunk is truncated!" #f)))
      (call-with-values (lambda () (k (integer->char char)))
        (lambda (continue? continuation)
          (if continue?
              (loop continuation (generator))
              (values (list->string (reverse chunk-extension))
                      (chunk-generator generator chunk-length))))))))

(define (generator/continuation generator k)
  (lambda ()
    (let ((out (generator)))
      (if (eof-object? out)
          (k)
          out))))

(define (http-chunked-body-generator generator)

  (define chunks)

  (define (next-chunk)
    (call-with-values chunks
      (lambda (extensions chunk-generator)
        (let ((maybe-eof (chunk-generator)))
          (if (eof-object? maybe-eof)
              (begin
                (set! continue eof-object)
                (eof-object))
              (begin
                (set! continue (generator/continuation chunk-generator next-chunk))
                maybe-eof))))))

  (define (start)
    (set! chunks (http-chunks-generator generator))
    (next-chunk))

  (define continue start)

  (lambda ()
    (continue)))

(define (http-chunk accumulator bytevector)
  (let* ((length (bytevector-length bytevector))
         (chunk-size (number->string length 16)))
    (put-string* accumulator chunk-size)
    (put-string* accumulator "\r\n")
    (let loop ((index 0))
      (unless (= (- length index) 0)
        (accumulator accumulator (bytevector-u8-ref bytevector index))
        (loop (+ index 1))))))

(define (string-trim-left string)
  (if (= (string-length string) 0)
      ""
      (if (char=? (string-ref string 0) #\space)
          (string-trim-left (substring string 1 (string-length string) ))
          string)))

(define (string-trim-right string)
  (if (= (string-length string) 0)
      ""
      (if (char=? (string-ref string (- (string-length string) 1)) #\space)
          (string-trim-right (substring string 0 (- (string-length string) 1)))
          string)))

(define (string-trim string)
  (string-trim-right (string-trim-left string)))

(define (content-length headers)
  (if (null? headers)
      #f
      (let ((head (string-downcase (caar headers))))
        (if (string=? head "content-length")
            (string->number (cdar headers))
            (content-length (cdr headers))))))

(define (chunked? headers)
  (if (null? headers)
      #f
      (let ((head (string-downcase (caar headers))))
        (if (string=? head (string-downcase "transfer-encoding"))
            (string=? (string-downcase (cdar headers))
                      "chunked")
            (chunked? (cdr headers))))))

(define (body-read headers generator)
  (cond
   ((content-length headers) => (lambda (n)
                                  (generator->string (gmap integer->char (gtake generator n)))))
   ((chunked? headers) (generator->string
                        (gmap integer->char
                              (http-chunked-body-generator generator))))
   (else #f)))

(define (http-request-read generator)
  (call-with-values (lambda () (http-request-line-read generator))
    (lambda (method uri version)
      (let ((headers (http-headers-read generator)))
        (values method uri version headers (body-read headers generator))))))

(define (http-request-write accumulator method uri version headers body)
  (http-request-line-write accumulator method uri version)
  (http-headers-write accumulator headers)
  (let* ((bytevector (string->utf8 body))
         (length (bytevector-length bytevector)))
    (let loop ((index 0))
      (unless (= (- length index) 0)
        (accumulator (bytevector-u8-ref bytevector index))
        (loop (+ index 1))))))

(define (http-response-read generator)
  (call-with-values (lambda () (http-response-line-read generator))
    (lambda (version code reason)
      (let ((headers (http-headers-read generator)))
        (values version code reason headers (body-read headers generator))))))

(define (http-response-write accumulator version status-code reason headers body)
  (http-response-line-write accumulator version status-code reason)
  (http-headers-write accumulator headers)
  (let* ((bytevector (string->utf8 body))
         (length (bytevector-length bytevector)))
    (let loop ((index 0))
      (unless (= (- length index) 0)
        (accumulator (bytevector-u8-ref bytevector index))
        (loop (+ index 1))))))
