(define-record-type Iterator
  (make-iterator peek next)
  iterator?
  (peek iterator-peek-proc)
  (next iterator->generator))

(define (iterator-peek iter)
  ((iterator-peek-proc iter)))

(define (iterator-next! iter)
  ((iterator->generator iter)))

(define (generator->iterator gen)
  (define next (gen))
  (make-iterator
   (lambda () next)
   (lambda () (let ((value next)) (set! next (gen)) value))))

(define (character-port->iterator port)
  (make-iterator
   (lambda () (peek-char port))
   (lambda () (read-char port))))

;; Split one generator into two,
;; allowing multiple operations on the same generator.
(define (gfork gen)
  (define left '())
  (define right '())
  (values
   (lambda ()
     (if (pair? left)
         (let ((next (car left)))
           (set! left (cdr left))
           next)
         (let ((next (gen)))
           (unless (eof-object? next)
             (set! right `(,@right ,next)))
           next)))
   (lambda ()
     (if (pair? right)
         (let ((next (car right)))
           (set! right (cdr right))
           next)
         (let ((next (gen)))
           (unless (eof-object? next)
             (set! left `(,@left ,next)))
           next)))))
