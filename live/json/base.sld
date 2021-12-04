(define-library (live json base)
  (export
   read
   quote
   let*
   begin
   fx+
   string->symbol
   number->string
   symbol?
   char?
   current-output-port
   include
   +
   -
   <=
   =
   and
   append
   ash
   bitwise-ior
   boolean?
   call-with-input-file
   call-with-port
   call-with-values
   car
   case
   case-lambda
   cdr
   char=?
   close-port
   cond
   cons
   current-input-port
   define
   define-record-type
   define-syntax
   denominator
   directory-list
   display
   eof-object?
   eq?
   equal?
   error
   exact?
   file-regular?
   for-all
   for-each
   format
   fx-
   fxzero?
   get-output-string
   guard
   if
   inexact?
   infinite?
   input-port?
   integer->char
   lambda
   length
   let
   list
   list->string
   make-parameter
   make-vector
   map
   nan?
   newline
   not
   null?
   number?
   open-input-string
   open-output-string
   or
   pair?
   pk
   procedure?
   put-char
   put-string
   raise
   read-char
   real?
   reverse
   set!
   string->number
   string-append
   string-for-each
   string?
   symbol->string
   textual-port?
   unless
   values
   vector-for-each
   vector-set!
   vector?
   void
   when
   write
   exit)
  (cond-expand
   ((or chicken gambit loko gauche)
    (import (scheme base)
            ;; (srfi 1)
            (scheme read)
            (scheme case-lambda)
            ;; (scheme bitwise)
            (scheme file)
            (scheme write)
            (scheme process-context)
            (live json shim)))
   (else
    (import (scheme base)
            (scheme list)
            (scheme read)
            (scheme case-lambda)
            (scheme bitwise)
            (scheme file)
            (scheme write)
            (scheme process-context)
            (live json shim))))

  (cond-expand
   (chicken
    (import (only (chicken bitwise) bitwise-ior)
            (only (srfi 1) every)))
   (loko
    (import (only (rnrs) bitwise-ior)))
   (else))

  (begin

    (cond-expand
     ((or gambit loko gauche)
      (define every
        (lambda (p? x)
          (if (null? x)
              #t
              (if (p? (car x))
                  (every (cdr x))
                  #f)))))
     (else))

    (cond-expand
     (loko
      (define (remove pred lis)
        (let recur ((lis lis))
          (if (null? lis) lis
              (let ((head (car lis))
                    (tail (cdr lis)))
                (if (not (pred head))
                    (let ((new-tail (recur tail)))
                      (if (eq? tail new-tail) lis
                          (cons head new-tail)))
                    (recur tail)))))))
     (else))


    (define fx+ +)
    (define fx- -)
    (define fxzero? zero?)

    (define nan? (lambda (x) #f))

    (define (format x message . args)
      (cons message args))

    (define for-all every)

    (define (void)
      (when #f #f))

    (define pk
      (lambda args
        ;; TODO: FIXME: Loko does like current-error-port
        (display ";; " #;(current-error-port))
        (write args #;(current-error-port))
        (car (reverse args))))

    (define ash arithmetic-shift)

    (define put-char (lambda (p c) (write-char c p)))

    (define put-string (lambda (p s) (write-string s p)))

    (define (infinite? x)
      (and (number? x)
           (or (equal? x +inf.0)
               (equal? x -inf.0))))))
