(library (live unstable)
  (export
   port?
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
   exclusive-cond
   file-regular?
   for-all
   for-each
   format
   fx-
   fxzero?
   get-output-string
   guard
   identifier?
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
  (import (rename (chezscheme) (define-record-type define-record-type*)))

  (define-syntax assume
    (syntax-rules ()
      ((assume expression message)
       (or expression
           (error 'assume message (quote expression))))
      ((assume . _)
       (syntax-error "invalid assume syntax"))))

  (define (pk . args)
    (write args (current-error-port))
    (newline (current-error-port))
    (car (reverse args)))

  (define-syntax define-record-type
    (lambda (stx)
      (syntax-case stx ()
        ((_ type (constructor constructor-tag ...)
            predicate
            (field-tag accessor setter ...) ...)
         (and (for-all identifier?
                       #'(type constructor constructor-tag ... predicate
                               field-tag ... accessor ... setter ... ...))
              (for-all (lambda (s) (<= 0 (length s) 1))
                       #'((setter ...) ...))
              (for-all (lambda (ct)
                         (memp (lambda (ft) (bound-identifier=? ct ft))
                               #'(field-tag ...)))
                       #'(constructor-tag ...)))
         (with-syntax (((field-clause ...)
                        (map (lambda (clause)
                               (if (= 2 (length clause))
                                   #`(immutable . #,clause)
                                   #`(mutable . #,clause)))
                             #'((field-tag accessor setter ...) ...)))
                       ((unspec-tag ...)
                        (remp (lambda (ft)
                                (memp (lambda (ct) (bound-identifier=? ft ct))
                                      #'(constructor-tag ...)))
                              #'(field-tag ...))))
                      #'(define-record-type* (type constructor predicate)
                          (protocol (lambda (ctor)
                                      (lambda (constructor-tag ...)
                                        (define unspec-tag) ...
                                        (ctor field-tag ...))))
                          (fields field-clause ...))))))))
