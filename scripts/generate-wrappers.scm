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

(define srfis
  '(151))

(define libraries
  '((live bitwise)
    (live number)
    (live string)
    (live time iso)))

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
          ,@(map library-name->sld libraries))
         (components
          ,@(map (lambda (lib)
                   `(extension
                     ,(library-name->chicken lib)
                     (source ,(library-name->sld lib))
                     (source-dependencies
                      ,@(map library-name->sld (mine 'include lib)))
                     (component-dependencies
                      ,@(map library-name->chicken (mine 'import lib)))
                     (csc-options "-R" "r7rs" "-X" "r7rs")))
                 libraries)))))))

(define (main)
  (write-chicken-5-egg-file))

(main)
