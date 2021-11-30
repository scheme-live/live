(define-syntax typecheck-bytevector
  (syntax-rules ()
    ((_ who var)
     (##sys#check-u8vector var 'who))))

(define-syntax typecheck-string
  (syntax-rules ()
    ((_ who var)
     (##sys#check-string var 'who))))
