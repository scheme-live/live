(library (live json srfi-9)

  (export define-record-type)

  (import (except (chezscheme) define-record-type)
          (rename (only (rnrs) define-record-type) (define-record-type define-record-type*)))

  (begin

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
                            (fields field-clause ...)))))))))
