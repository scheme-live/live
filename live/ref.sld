(define-library (live ref)
  (export ref)
  (import (scheme base) (live list) (live hash-table))
  (begin

    (define ref-types '())
    (define ref-type-matcher car)
    (define ref-type-getter  cdr)
    (define (define-ref-type matcher getter)
      (set! ref-types (append ref-types (list (cons matcher getter)))))

    (define (ref-once obj key)
      (let loop ((types ref-types))
        (if (null? types) (error "Don't know how to ref" obj)
            (let ((type (car types)))
              (if ((ref-type-matcher type) obj)
                  ((ref-type-getter  type) obj key)
                  (loop (cdr types)))))))

    (define (ref obj . keys)
      (if (null? keys) obj (apply ref (ref-once obj (car keys)) (cdr keys))))

    ;;

    (define-ref-type vector? vector-ref)
    (define-ref-type hash-table? hash-table-ref)
    (define-ref-type
      (lambda (obj) (or (null? obj) (pair? obj)))
      (lambda (obj key)
        (if (number? key) (list-ref obj key)
            (let ((pair (assoc key obj)))
              (if pair (cdr pair)
                  (error "Key not found in alist" key obj))))))))
