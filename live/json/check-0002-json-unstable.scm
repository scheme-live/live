#!/usr/bin/env -S scheme-live current run
(import (live json base))
(import (live json unstable))


(define (json->obj->json->obj filepath)
  ;; Take in JSON by filepath, convert it a scheme object; then
  ;; convert it to JSON string with json-write, and json-read it back
  ;; to a Scheme object. This is made so, to be sure it is possible to
  ;; read original JSON text, and that what json-write produce can
  ;; also be read.

  ;; The output is a Scheme object

  (define (call-with-input-string string proc)
    (call-with-port (open-input-string string) proc))

  (define (call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (let ((string (get-output-string port)))
        (close-port port)
        string)))

  (call-with-input-string
   (call-with-output-string
    (lambda (port)
      (json-write (call-with-input-file filepath json-read) port)))
   (lambda (port)
     (json-read port))))

(define data "./live/json/data")
(define checks (directory-list data))

(define (dodo name)

  (define input (string-append data "/" name "/input.json"))
  (define output (string-append data "/" name "/output.scm"))
  (define error (string-append data "/" name "/error.txt"))

  (define (on-error exc)
    ;; sloppy: check that there is any error, if it is the case pass
    ;; the test
    (if (file-regular? error)
        #t
        (begin
          (newline)
          (write exc)
          (newline)
          #f)))


  (display "* ")(display name) (display " => ")

  (let ((out (guard (error (else (on-error error)))
               (let ((obj (json->obj->json->obj input)))
                 (equal? obj (call-with-input-file output read))))))
    (display out)
    (newline)
    out))

(define (every predicate? objects)
  (if (null? objects)
      #t
      (if (predicate? (car objects))
          (every predicate? (cdr objects))
          #f)))

(exit (if (every values (map dodo checks)) 0 1))
