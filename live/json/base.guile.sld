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
   bound-identifier=?
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
   current-directory
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
   memp
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
   remp
   reverse
   set!
   string->number
   string-append
   string-for-each
   string?
   symbol->string
   syntax-case
   textual-port?
   unless
   values
   vector-for-each
   vector-set!
   vector?
   void
   when
   with-syntax
   write
   exit)
  (import (except (guile) else case)
          (rename
           (only (rnrs) case else => textual-port? vector-for-each infinite?)
           (case r6:case))
          (only (rnrs exceptions) guard)
          (only (rnrs arithmetic fixnums) fxzero? fx- fx+)
          (only (rnrs arithmetic bitwise) bitwise-ior)
          (only (rnrs io ports) put-char put-string)
          (only (scheme base) define-record-type call-with-port)
          (live json guile))

  (begin

    (define (void)
      (when #f #f))

    (define-syntax %r7case-clause
      (syntax-rules (else =>)
        ((_ obj (translated ...) ())
         (r6:case obj translated ...))
        ((_ obj (translated ...) (((e0 e1 ...) => f) rest ...))
         (%r7case-clause obj (translated ... ((e0 e1 ...) (f obj))) (rest ...)))
        ((_ obj (translated ...) ((else => f) rest ...))
         (%r7case-clause obj (translated ... (else (f obj))) (rest ...)))
        ((_ obj (translated ...) (otherwise rest ...))
         (%r7case-clause obj (translated ... otherwise) (rest ...)))))

    (define-syntax case
      (syntax-rules (else =>)
        ((_ key clause ...)
         (let ((obj key))
           (%r7case-clause obj () (clause ...))))))))
