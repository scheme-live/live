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
   else
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
  (import (scheme base)
          (scheme list)
          (scheme read)
          (scheme case-lambda)
          (scheme bitwise)
          (scheme file)
          (scheme write)
          (scheme process-context)
          (live json shim))

  (begin

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
        (display ";; ")
        (write args (current-error-port))
        (car (reverse args))))

    (define ash arithmetic-shift)

    (define put-char (lambda (p c) (write-char c p)))

    (define put-string (lambda (p s) (write-string s p)))

    (define (infinite? x)
      (and (number? x)
           (or (equal? x +inf.0)
               (equal? x -inf.0))))))