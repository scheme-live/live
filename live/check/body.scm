(define (pk . args)
  (display ";; " (current-error-port))
  (write args (current-error-port))
  (display #\newline (current-error-port))
  (car (reverse args)))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (keyword args ...) body)
     (define-syntax keyword
       (syntax-rules ()
         ((keyword args ...) body))))))

(define-syntax check
  (syntax-rules ()
    ((check predicate? expected actual)
     (lambda ()
       (let ((expected* expected)
             (actual* actual))
         (if (predicate? expected* actual*)
             (vector #t)
             (vector #f 'unexpected-value expected* actual*)))))
    ((check expected actual)
     (check equal? expected actual))
    ((check actual)
     (check (lambda (x y) (if x #t #f)) #t actual))
    ((check predicate? v0 v1 ...)
     (check (predicate? v0 v1 ...)))))

(define-syntax check-raise
  (syntax-rules ()
    ((check-raise predicate? expr)
     (lambda ()
       (let ((predicate?* predicate?))
         (guard (ex ((predicate?* ex) (vector #t))
                    (else (vector #f 'unexpected-exception predicate?* ex)))
           (let ((expr* expr))
             (vector #f 'no-exception predicate?* expr*))))))
    ((check-raise expr)
     (check-raise (lambda (x) #t) expr))))

(define-syntax-rule (check-skip test expected actual)
  (lambda ()
    (vector #t)))

(define-syntax-rule (check-values expected actual)
  (check (call-with-values (lambda () expected)
           (lambda args args))
         (call-with-values (lambda () actual)
           (lambda args args))))

;; how to run the test suite

(define (error-format vector)
  (case (vector-ref vector 1)
    ((unexpected-value)
     (display "*** expected: ") (write (vector-ref vector 2)) (newline)
     (display "*** but have: ") (write (vector-ref vector 3)) (newline))
    (else (write vector))))

(define-syntax-rule (run-one-check library-name check-name check)
  (guard (obj
          ((condition? obj)
           (display (vector #f
                            'error
                            (list (condition-message obj)
                                  (condition-irritants obj))))
           (newline)
           (exit 1))
          (else (display (vector #f 'error obj))
                (newline)
                (exit 1)))
    (display "** ")
    (display 'library-name)
    (display " ")
    (display 'check-name)
    (newline)
    (let ((out (check)))
      (when (not (vector-ref out 0))
        (error-format out)
        (newline)
        (exit 1)))))

(define (main args)

  (define (string-suffix? string suffix)
    (let loop ((path (reverse (string->list string)))
               (suffix (reverse (string->list suffix))))
      (if (null? suffix)
          #t
          (if (char=? (car path) (car suffix))
              (loop (cdr path) (cdr suffix))
              #f))))

  (define (check-library? path)
    (and (string-suffix? path "/check.scm")
         (not (string-suffix? path "live/check.scm"))))

  (define (directory-list* path)
    ;; list absolute files and directory names at PATH
    (map (lambda (item) (string-append path "/" item))
         (directory-list path)))

  (define (call-with-input-file path proc)
    (define port (open-input-file path))
    (call-with-values (lambda () (proc port))
      (lambda args
        (close-port port)
        (apply values args))))

  (define (discover-checks path)
    (call-with-input-file path
      (lambda (port)
        (let* ((sexp (read port)))
          ;; library-name and exports
          (cons (cadr sexp) (cdaddr sexp))))))

  (define (discover)
    (let loop ((paths (directory-list* (current-directory)))
               (out '()))
      (if (null? paths)
          out
          (let ((path (car paths)))
            (if (file-directory? path)
                (loop (append (cdr paths) (directory-list* path))
                      out)
                (if (check-library? path)
                    (loop (cdr paths)
                          (cons (discover-checks path) out))
                    (loop (cdr paths) out)))))))

  (define (discover-one path check-name)
    (call-with-input-file path
      (lambda (port)
        (let* ((sexp (read port)))
          ;; library-name
          (list (cons (cadr sexp)
                      (list (string->symbol check-name))))))))

  (define (make-check-program libraries)

    (define (make-prefix name index)
      (string->symbol
       (string-append "checks-" (number->string index) ":")))

    (define prefixes (map make-prefix
                          libraries
                          (iota (length libraries))))

    (define (symbol-append a b)
      (string->symbol
       (string-append (symbol->string a)
                      (symbol->string b))))

    (define (run-one-check-sexp spec)
      `(run-one-check ',(car spec)
                      ',(caddr spec)
                      ,(symbol-append (cadr spec) (caddr spec))))

    (define (import-check-library-sexp library-name prefix)
      `(prefix ,library-name ,prefix))

    (define (massage libraries prefixes)
      (let loop0 ((libraries libraries)
                  (prefixes prefixes)
                  (out '()))
        (if (null? libraries)
            (reverse out)
            (let loop1 ((checks (cdar libraries))
                        (out out))
              (if (null? checks)
                  (loop0 (cdr libraries)
                         (cdr prefixes)
                         out)
                  (loop1 (cdr checks)
                         (cons (list (caar libraries)
                                     (car prefixes)
                                     (car checks))
                               out)))))))

    `((import (live check)
              ,@(map import-check-library-sexp
                     (map car libraries)
                     prefixes))

      ,@(map run-one-check-sexp(massage libraries prefixes))))

  (define (exec alist)
    (define env (copy-environment (environment '(chezscheme)) #t))
    (let loop ((program (make-check-program alist)))
      (unless (null? program)
        (eval (car program) env)
        (loop (cdr program)))))

  (cond
   ((null? args) (exec (discover)))
   ((null? (cdr args)) (exec (list (discover-checks (car args)))))
   ((null? (cddr args)) (exec (discover-one (car args) (cadr args))))
   (else (error 'check-runner "unknown argument format"))))
