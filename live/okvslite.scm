#!chezscheme
(library (db)
  (export
   db-call-with-cursor
   db-cursor-seek?
   db-cursor-next?
   db-cursor-previous?
   db-query)
  (import (chezscheme))

  (define (pk . args)
    (write args)
    (newline)
    (car (reverse args)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-syntax-rule (bytevector-pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define-syntax-rule (with-lock objs body ...)
    (begin
      (for-each lock-object objs)
      (call-with-values (lambda () body ...)
        (lambda args
          (for-each unlock-object objs)
          (apply values args)))))

  ;; ffi helpers

  (define-syntax-rule (make-double-pointer)
    (bytevector-pointer (make-bytevector 8)))

  (define-syntax-rule (dereference  pointer)
    (foreign-ref 'void* pointer 0))

  (define-syntax-rule (ftype->pointer ftype)
    (ftype-pointer-address ftype))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure __collect_safe ptr (args ...) return))

  ;; sqlite lsm extension bindings

  (define okvslite (load-shared-object "./local/lib/lsm.so"))

  (define (error->message code)
    ;; TODO: replace with a switch with fx=?
    (case code
      ((1) "ERROR")
      ((5) "BUSY")
      ((7) "NO MEMORY")
      ((8) "READ ONLY")
      ((10) "IO ERROR")
      ((11) "CORRUPT")
      ((13) "FULL")
      ((14) "CAN NOT OPEN")
      ((15) "PROTOCOL")
      ((21) "MISUSE")
      ((50) "MISMATCH")
      (else "UNKNOWN ERROR")))

  (define-syntax-rule (check caller code)
    (let ((code* code))
      (unless (zero? code*)
        (error 'okvslite (error->message code*) caller code*))))

  (define okvslite-new
    (let ((proc (foreign-procedure* int "lsm_new" void* void*)))
      (lambda ()
        (let ((out (make-double-pointer)))
          (check 'okvslite-new (proc 0 out))
          (dereference out)))))

  (define okvslite-close
    (let ((proc (foreign-procedure* int "lsm_close" void*)))
      (lambda (db)
        (check 'okvslite-close (proc db)))))

  ;; TODO: replace VALUE with a list of symbols
  ;; TOOD: make sure it works
  (define okvslite-config
    (let ((proc (foreign-procedure* int "lsm_config" void* int void*)))
      (lambda (db config value)
        (let ((pointer (make-double-pointer)))
          (foreign-set! 'int pointer 0 value)
          (check 'okvslite-config (proc db config pointer))))))

  (define okvslite-open
    (let ((proc (foreign-procedure "lsm_open" (void* string) int)))
      (lambda (db filename)
        (check 'okvslite-open (proc db filename)))))

  (define okvslite-begin
    (let ((proc (foreign-procedure* int "lsm_begin" void* int)))
      (lambda (db level)
        (check 'okvslite-begin (proc db level)))))

  (define okvslite-commit
    (let ((proc (foreign-procedure* int "lsm_commit" void* int)))
      (lambda (db level)
        (check 'okvslite-commit (proc db level)))))

  (define okvslite-rollback
    (let ((proc (foreign-procedure* int "lsm_rollback" void* int)))
      (lambda (db level)
        (check 'okvslite-rollback (proc db level)))))

  (define okvslite-insert
    (let ((proc (foreign-procedure* int "lsm_insert" void* void* int void* int)))
      (lambda (db key value)
        (with-lock (list key value)
          (check 'okvslite-insert
                 (proc db
                       (bytevector-pointer key)
                       (bytevector-length key)
                       (bytevector-pointer value)
                       (bytevector-length value)))))))

  (define okvslite-delete
    (let ((proc (foreign-procedure* int "lsm_delete" void* void* int)))
      (lambda (db key)
        (with-lock (list key)
          (check 'okvslite-delete
                 (proc db
                       (bytevector-pointer key)
                       (bytevector-length key)))))))

  (define okvslite-cursor-open
    (let ((proc (foreign-procedure* int "lsm_csr_open" void* void*)))
      (lambda (db)
        (let ((out (make-double-pointer)))
          (check 'okvslite-cursor-open (proc db out))
          (dereference out)))))

  (define okvslite-cursor-close
    (let ((proc (foreign-procedure* int "lsm_csr_close" void*)))
      (lambda (cursor)
        (check 'okvslite-cursor-close (proc cursor)))))

  (define okvslite-cursor-seek
    (let ((proc (foreign-procedure* int "lsm_csr_seek" void* void* int int)))
      (lambda (cursor key strategy)

        (define (->seek symbol)
          (case symbol
            ((less-than-or-equal-fast) -2)
            ((less-than-or-equal) -1)
            ((equal) 0)
            ((greater-than-or-equal) 1)
            (else (error 'okvslite "unknown seek strategy"))))

        (with-lock (list key)
          (check 'okvslite-cursor-seek
                 (proc cursor
                       (bytevector-pointer key)
                       (bytevector-length key)
                       (->seek strategy)))))))

  (define okvslite-cursor-first
    (let ((proc (foreign-procedure* int "lsm_csr_first" void*)))
      (lambda (cursor)
        (check 'okvslite-cursor-first (proc cursor)))))

  (define okvslite-cursor-last
    (let ((proc (foreign-procedure* int "lsm_csr_last" void*)))
      (lambda (cursor)
        (check 'okvslite-cursor-last (proc cursor)))))

  (define okvslite-cursor-next
    (let ((proc (foreign-procedure* int "lsm_csr_next" void*)))
      (lambda (cursor)
        (check 'okvslite-cursor-next (proc cursor)))))

  (define okvslite-cursor-prev
    (let ((proc (foreign-procedure* int "lsm_csr_prev" void*)))
      (lambda (cursor)
        (check 'okvslite-cursor-prev (proc cursor)))))

  (define okvslite-cursor-valid?
    (let ((proc (foreign-procedure* int "lsm_csr_valid" void*)))
      (lambda (cursor)
        (fx=? (proc cursor) 1))))

  (define okvslite-cursor-key
    (let ((proc (foreign-procedure* int "lsm_csr_key" void* void* void*)))
      (lambda (cursor)
        ;; TODO: replace with a fixed size bytevector
        (let ((data* (make-double-pointer))
              (length* (make-double-pointer)))
          (check 'okvslite-cursor-key (proc cursor data* length*))
          ;; copy the data into a scheme bytevector
          (let* ((data (dereference data*))
                 (length (foreign-ref 'int length* 0))
                 (bytevector (make-bytevector length)))
            (let loop ((index (- length 1)))
              (unless (< index 0)
                (let ((value (foreign-ref 'unsigned-8 data index)))
                  (bytevector-u8-set! bytevector index value)
                  (loop (- index 1)))))
            bytevector)))))

  (define okvslite-cursor-value
    (let ((proc (foreign-procedure* int "lsm_csr_value" void* void* void*)))
      (lambda (cursor)
        ;; TODO: replace with fixed size bytevector
        (let ((data* (make-double-pointer))
              (length* (make-double-pointer)))
          (check 'okvslite-cursor-value (proc cursor data* length*))
          ;; copy the data into a scheme bytevector
          (let* ((data (dereference data*))
                 (length (foreign-ref 'int length* 0))
                 (bytevector (make-bytevector length)))
            (let loop ((index (- length 1)))
              (unless (< index 0)
                (let ((value (foreign-ref 'unsigned-8 data index)))
                  (bytevector-u8-set! bytevector index value)
                  (loop (- index 1)))))
            bytevector)))))

  (define (db-open filename)
    (define okvs (okvslite-new))
    (okvslite-open okvs filename)
    okvs)

  (define (db-cursor-seek? cursor key strategy)
    (okvslite-cursor-seek cursor key strategy)
    (okvslite-cursor-valid? cursor))

  (define (db-cursor-next? cursor)
    (okvslite-cursor-next cursor)
    (okvslite-cursor-valid? cursor))

  (define (db-cursor-previous? cursor)
    (okvslite-cursor-prev cursor)
    (okvslite-cursor-valid? cursor))

  (define (failure-default)
    (error 'okvs "transaction failed"))

  (define (db-in-transaction% okvs proc failure values)
    (guard (ex
            (else
             (okvslite-rollback okvs 0)
             (failure)))
      (okvslite-begin okvs 0)
      (call-with-values (lambda () (proc okvs))
        (lambda args
          (okvslite-commit okvs 0)
          (apply values args)))))

  (define db-in-transaction
    (case-lambda
     ((okvs proc)
      (db-in-transaction okvs proc failure-default values))
     ((okvs proc failure) (db-in-transaction okvs proc failure values))
     ((okvs proc failure success) (db-in-transaction% okvs proc failure values))))

  (define (db-call-with-cursor okvs proc)
    (define cursor (okvslite-cursor-open okvs))
    (guard (ex (else (okvslite-cursor-close cursor) (raise ex)))
      (call-with-values (lambda ()
                          (proc cursor))
        (lambda args
          (okvslite-cursor-close cursor)
          (apply values args)))))

  (define db-set! okvslite-insert)

  (define (db-ref okvs key)
    (db-call-with-cursor okvs
      (lambda (cursor)
        (okvslite-cursor-seek cursor key 'equal)
        (if (okvslite-cursor-valid? cursor)
            (okvslite-cursor-value cursor)
            #f))))

  (define (compare bytevector other)
    ;; lexicographic comparison

    ;; If BYTEVECTOR is before OTHER return 'smaller, if equal return
    ;; 'same, otherwise if BYTEVECTOR is after OTHER return 'bigger
    (let ((end (min (bytevector-length bytevector)
                    (bytevector-length other))))
      (let loop ((index 0))
        (if (fx=? (fx- end index) 0)
            (if (= (bytevector-length bytevector)
                   (bytevector-length other))
                'same
                (if (fx< (bytevector-length bytevector)
                         (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fx=? delta 0)
                  (loop (+ 1 index))
                  (if (fx<? delta 0)
                      'smaller
                      'bigger)))))))

  (define (db-generator okvs key other offset limit)
    (define cursor (okvslite-cursor-open okvs))
    (define count 0)

    (define (fini!) (okvslite-cursor-close cursor) (set! yield eof-object) (eof-object))

    ;; go through the range in lexicographic order

    (define (init)
      (if (not (db-cursor-seek? cursor key 'greater-than-or-equal))
          (fini!)
          (begin
            (if (or limit offset)
                (begin
                  (set! count (fx+ count 1))
                  (set! yield continue/offset+limit)
                  (if (and offset (fx>? offset 0))
                      (continue/offset+limit)
                      (maybe-continue)))
                (begin
                  (set! yield continue)
                  (maybe-continue))))))

    (define (maybe-continue)
      (let ((key* (okvslite-cursor-key cursor)))
        (case (compare key* other)
          ((smaller) (cons key* (okvslite-cursor-value cursor)))
          (else (fini!)))))

    (define (continue)
      (if (not (db-cursor-next? cursor))
          (fini!)
          (maybe-continue)))

    (define (continue/offset+limit)
      (if (and limit (fx>? count limit))
          (fini!)
          (if (not (db-cursor-next? cursor))
              (fini!)
              (begin
                (set! count (fx+ count 1))
                (if (fx>? count offset)
                    (maybe-continue)
                    (continue/offset+limit))))))

    (define yield init)

    (lambda ()
      (yield)))

  (define (db-reverse-generator okvs key other offset limit)
    (define cursor (okvslite-cursor-open okvs))
    (define count 0)

    (define (fini!) (okvslite-cursor-close cursor) (set! yield eof-object) (eof-object))

    ;; go through the range in reverse order

    (define (init2)
      (if (or limit offset)
          (begin
            (if (and offset (fx>=? offset 0))
                (begin
                  (set! yield continue/offset+limit)
                  (continue/offset+limit))
                (maybe-continue)))
            (begin
              (set! yield continue)
              (set! count (fx+ count 1))
              (maybe-continue))))

    (define (init)
      (if (not (db-cursor-seek? cursor other 'less-than-or-equal))
          (fini!)
          (let ((key* (okvslite-cursor-key cursor)))
            (if (eq? (compare key other) 'same)
                (if (not (db-cursor-previous? cursor))
                    (fini!)
                    (init2))
                (init2)))))

    (define (maybe-continue)
      (let ((key* (okvslite-cursor-key cursor)))
        (case (compare key* key)
          ((smaller) (fini!))
          (else (cons key* (okvslite-cursor-value cursor))))))

    (define (continue)
      (if (not (db-cursor-previous? cursor))
          (fini!)
          (maybe-continue)))

    (define (continue/offset+limit)
      (if (and limit (fx=? count limit))
          (fini!)
          (if (not (db-cursor-previous? cursor))
              (fini!)
              (begin
                (set! count (fx+ count 1))
                (maybe-continue)))))

    (define yield init)

    (lambda ()
      (yield)))

  (define db-query
    (case-lambda
     ((okvs key) (db-ref okvs key))
     ((okvs key other) (db-query okvs key other #f #f))
     ((okvs key other offset limit)
      (assert (or (not limit) (fx>? limit 0)))
      (case (compare key other)
        ((smaller) (db-generator okvs key other offset limit))
        (else (db-reverse-generator okvs other key offset limit))))))


  (define (generator->list generator)
    (let loop ((out '()))
      (let ((item (generator)))
        (if (eof-object? item)
            (reverse out)
            (loop (cons item out))))))

  (define (generator-map proc generator)
    (lambda ()
      (proc (generator))))

  (define db-delete! okvslite-delete)

  (define (generator-for-each proc generator)
    (let loop ((item (generator)))
      (unless (eof-object? item)
        (proc item)
        (loop (generator)))))

  (define (db-remove!% okvs key other)
    (define (delete! key+value)
      (db-delete! okvs (car key+value)))

    ;; TODO: replace with call-with-cursor, and avoid db-delete! to
    ;; ease GC.
    (generator-for-each delete! (db-query okvs key other)))

  (define db-remove!
    (case-lambda
     ((okvs key) (db-delete! okvs key))
     ((okvs key other) (db-remove!% okvs key other))))

  (define db-close okvslite-close))
