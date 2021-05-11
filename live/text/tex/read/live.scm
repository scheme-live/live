(define tex-escape-char (make-parameter #\\))

(define (tex-command-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)))

(define (not-tex-special-char? ch)
  (not (or (eqv? ch #\{)
           (eqv? ch #\})
           (eqv? ch (tex-escape-char)))))

(define (tex-read-command-args)
  (let loop ((args '()))
    (if (not (read-char? #\{))
        args
        (loop (append args (list (tex-read-until #\})))))))

(define (tex-read-thing)
  (cond ((read-char? (tex-escape-char))
         (let ((command (read-char* tex-command-char?)))
           (if command
               (cons (string->symbol command)
                     (tex-read-command-args))
               (read-char* not-tex-special-char?))))
        ((read-char? #\{)
         (cons 'math (tex-read-until #\})))
        (else (read-char* not-tex-special-char?))))

(define (tex-read-until sentinel)
  (let loop ((things '()))
    (if (read-char? sentinel)
        things
        (let ((thing (tex-read-thing)))
          (if (not thing)
              things
              (loop (append things (list thing))))))))

(define (tex-read-document)
  (tex-read-until eof-object?))
