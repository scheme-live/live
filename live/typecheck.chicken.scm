(define-syntax typecheck-string
  (syntax-rules ()
    ((_ who var)
     (##sys#check-string var 'who))))
