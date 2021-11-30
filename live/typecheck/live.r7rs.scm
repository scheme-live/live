(define-syntax typecheck-bytevector
  (syntax-rules ()
    ((_ who var)
     (unless (bytevector? var)
       (error "Not a bytevector" var 'who)))))

(define-syntax typecheck-string
  (syntax-rules ()
    ((_ who var)
     (unless (string? var)
       (error "Not a string" var 'who)))))
