(define-library (live unstable)
  (export
   ...
   _
   list?
   syntax-rules
   let-syntax
   assume
   test
   port?
   read
   let*
   letrec
   letrec*
   odd?
   even?
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
   define-record-type
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
   inexact?
   infinite?
   input-port?
   integer->char
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
   exit
   odd?
   even?)

  (cond-expand
   (chicken
    (export set!)
    (export lambda)
    (export if)
    (export define-syntax)
    (export define)
    (export quote))
   (cyclone
    (export vector make-record-marker))
   (else
    (export set!)
    (export lambda)
    (export if)
    (export define-syntax)
    (export define)
    (export quote)
    (export else)))

  (cond-expand
   ((or chicken gambit loko gauche mit cyclone)
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
   (mit
    (import (rename (srfi 143) (fxior bitwise-ior))))
   (cyclone
    (import (only (srfi 1) every))
    (import (only (srfi 60) bitwise-ior)))
   (else))

  (begin

    (define-syntax assume
      (syntax-rules ()
        ((assume expression message)
         (or expression
             (error 'assume message (quote expression))))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))

    (define-syntax test
      (syntax-rules ()
        ((test expected expression)
         (guard (_ (else (exit 255)))
                (if (equal? expected expression)
                    (exit 0)
                    (exit 255))))))
    
    (cond-expand
     ((or gambit loko mit gauche)
      (define every
        (lambda (p? x)
          (if (null? x)
              #t
              (if (p? (car x))
                  (every (cdr x))
                  #f)))))
     (chicken)
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

    (cond-expand
     (mit
      (define ignorable values))
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

    (cond-expand
     (loko
      (define pk
        (lambda args
          (display ";; ")
          (write args)
          (car (reverse args)))))
     (else
      (define pk
        (lambda args
          (display ";; " (current-error-port))
          (write args (current-error-port))
          (car (reverse args))))))


    (define ash arithmetic-shift)

    (define put-char (lambda (p c) (write-char c p)))

    (define put-string (lambda (p s) (write-string s p)))

    (define (infinite? x)
      (and (number? x)
           (or (equal? x +inf.0)
               (equal? x -inf.0))))))
