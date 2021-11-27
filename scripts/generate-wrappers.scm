#!

;;! Copyright 2021 Lassi Kortela
;;! SPDX-License-Identifier: MIT

(import (scheme base) (scheme write) (srfi 1) (srfi 193))

(cond-expand
  (chicken
   (import (only (chicken pretty-print) pretty-print))))

(define live-root (string-append (script-directory) "../"))

(define title "Scheme Live")
(define author "Scheme Live Crew")
(define tagline "fast-moving library collection with stable releases")
(define spdx-license-expression "MIT")

;; TODO: These SRFI numbers should be found by groveling the imports
;; in the .sld files.
(define srfis
  '(143
    151))

(define make-library cons)
(define library-name-parts car)
(define library-versions cdr)

(define (library-names/r6rs lib)
  (map (lambda (ver)
         `(live ,@(library-name-parts lib)
                ,(if (number? ver)
                     (string->symbol
                      (string-append "v" (number->string ver)))
                     ,ver)))
       (library-versions lib)))

(define (library-names/r7rs lib)
  (map (lambda (ver)
         `(live ,@(library-name-parts lib)
                ,ver))
       (library-versions lib)))

(define libraries
  (list

   (make-library '(bitwise)
                 '(unstable))

   (make-library '(fixnum)
                 '(unstable))

   (make-library '(list)
                 '(unstable))

   (make-library '(number)
                 '(unstable))

   (make-library '(string)
                 '(unstable))

   (make-library '(time iso)
                 '(unstable))))

(define (string-join lst delimiter)
  (if (null? lst) ""
      (fold (lambda (item result) (string-append result delimiter item))
            (car lst) (cdr lst))))

(define (lnp->string part)
  ((if (number? part) number->string symbol->string) part))

(define (library-name->sld parts)
  (string-append (string-join (map lnp->string parts) "/") ".sld"))

(define (library-name->chicken parts)
  (string->symbol (string-join (map lnp->string parts) ".")))

(define (mine x y)
  '())

(define (disp . xs) (for-each display xs) (newline))

(define (write-chicken-5-egg-file)
  (disp "Writing live.egg")
  (with-output-to-file (string-append live-root "live.egg")
    (lambda ()
      (disp ";;! Emacs: -*- Scheme -*-")
      (disp ";;! Generator: " (command-name))
      (disp)
      (disp ";; This file defines the Chicken 5 egg."
            " It is used by chicken-install.")
      (disp)
      (pretty-print
       `((synopsis ,(string-append title ": " tagline))
         (category misc)
         (license ,spdx-license-expression)
         (author ,author)
         (dependencies
          ,@(map (lambda (srfi)
                   (string->symbol
                    (string-append "srfi-" (number->string srfi))))
                 srfis))
         (test-dependencies)
         (distribution-files
          "live.egg"
          "live.release-info"
          ,@(map library-name->sld (append-map library-names/r7rs libraries)))
         (components
          ,@(append-map
             (lambda (lib)
               (map (lambda (libname)
                      `(extension
                        ,(library-name->chicken libname)
                        (source ,(library-name->sld libname))
                        (source-dependencies
                         ,@(map library-name->sld (mine 'include lib)))
                        (component-dependencies
                         ,@(map library-name->chicken (mine 'import lib)))
                        (csc-options "-R" "r7rs" "-X" "r7rs")))
                    (library-names/r7rs lib)))
             libraries)))))))

(define (main)
  (write-chicken-5-egg-file))

(main)
