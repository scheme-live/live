(define-library (live json base)
  (export
   port?
   read
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
   exit)

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

    (cond-expand
     ((or gambit loko mit gauche)
      (define every
        (lambda (p? x)
          (if (null? x)
              #t
              (if (p? (car x))
                  (every (cdr x))
                  #f)))))

     (cyclone
      (define * *)
      (define + +)
      (define - -)
      (define / /)
      (define < <)
      (define <= <=)
      (define = =)
      (define > >)
      (define >= >=)
      (define apply apply)
      (define boolean? boolean?)
      (define bytevector bytevector)
      (define bytevector-append bytevector-append)
      (define bytevector-length bytevector-length)
      (define bytevector-u8-ref bytevector-u8-ref)
      (define bytevector-u8-set! bytevector-u8-set!)
      (define bytevector? bytevector?)
      (define caar caar)
      (define cadr cadr)
      (define car car)
      (define cdar cdar)
      (define cddr cddr)
      (define cdr cdr)
      (define char->integer char->integer)
      (define char? char?)
      (define close-input-port close-input-port)
      (define close-output-port close-output-port)
      (define close-port close-port)
      (define command-line-arguments command-line-arguments)
      (define cons cons)
      (define delete-file delete-file)
      (define eof-object? eof-object?)
      (define eq? eq?)
      (define equal? equal?)
      (define eqv? eqv?)
      (define error error)
      (define exit exit)
      (define file-exists? file-exists?)
      (define integer->char integer->char)
      (define integer? integer?)
      (define length length)
      (define list->string list->string)
      (define list->vector list->vector)
      (define make-bytevector make-bytevector)
      (define make-vector make-vector)
      (define null? null?)
      (define number->string number->string)
      (define number? number?)
      (define open-input-file open-input-file)
      (define open-output-file open-output-file)
      (define pair? pair?)
      (define peek-char peek-char)
      (define port? port?)
      (define procedure? procedure?)
      (define read-char read-char)
      (define real? real?)
      (define set-car! set-car!)
      (define set-cdr! set-cdr!)
      (define string->number string->number)
      (define string->symbol string->symbol)
      (define string-append string-append)
      (define string-cmp string-cmp)
      (define string-length string-length)
      (define string-ref string-ref)
      (define string-set! string-set!)
      (define string? string?)
      (define substring substring)
      (define symbol->string symbol->string)
      (define symbol? symbol?)
      (define system system)
      (define vector-length vector-length)
      (define vector-ref vector-ref)
      (define vector-set! vector-set!)
      (define vector? vector?))

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
