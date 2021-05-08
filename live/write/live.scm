(define (disp . objs)
  (for-each display objs)
  (newline))

(define (displayed obj)
  (with-output-to-string (lambda () (display obj))))

(define (displayln obj)
  (display obj)
  (newline))

(define (edisp . objs)
  (parameterize ((current-output-port (current-error-port)))
    (apply disp objs)))

(define (writeln obj)
  (write obj)
  (newline))

(define (written obj)
  (with-output-to-string (lambda () (write obj))))
