#!

;;! Copyright 2021 Lassi Kortela
;;! SPDX-License-Identifier: MIT

(import (scheme base) (scheme write)
        (srfi 1) (srfi 13) (srfi 132) (srfi 193))

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

(define chicken-eggs
  (append '(r7rs
            openssl
            uri-generic)
          (map (lambda (srfi)
                 (string->symbol
                  (string-append "srfi-" (number->string srfi))))
               srfis)))

(define make-library cons)
(define library-name-parts car)
(define library-versions cdr)

(define (library-names/r6rs lib)
  (map (lambda (ver)
         `(live ,@(library-name-parts lib)
                ,(if (number? ver)
                     (string->symbol
                      (string-append "v" (number->string ver)))
                     ver)))
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

   (make-library '(net gemini)
                 '(unstable))

   (make-library '(net gemini client)
                 '(unstable))

   (make-library '(number)
                 '(unstable))

   (make-library '(string)
                 '(unstable))

   (make-library '(time iso)
                 '(unstable))))

(define (list<? elem<? list1 list2)
  (cond ((null? list1) (not (null? list2)))
        ((null? list2) #f)
        ((elem<? (car list1) (car list2)) #t)
        ((elem<? (car list2) (car list1)) #f)
        (else (list<? elem<? (cdr list1) (cdr list2)))))

(define (tree-fold merge state tree)
  (let ((state (merge tree state)))
    (if (not (list? tree)) state
        (fold (lambda (elem state) (tree-fold merge state elem))
              state tree))))

(define (lnp->string part)
  ((if (number? part) number->string symbol->string) part))

(define (library-name->directory parts)
  (fold (lambda (part whole) (string-append whole part "/"))
        "" (map lnp->string (drop-right parts 1))))

(define (library-name->sld parts)
  (string-append (string-join (map lnp->string parts) "/") ".sld"))

(define (library-name->chicken parts)
  (string->symbol (string-join (map lnp->string parts) ".")))

(define (library-name-part<? part1 part2)
  (define (stringify part)
    (if (number? part)
        (number->string part)
        (symbol->string part)))
  (string<? (stringify part1)
            (stringify part2)))

(define (library-name<? name1 name2)
  (list<? library-name-part<? name1 name2))

(define (list-with-head? symbol obj)
  (and (list? obj) (not (null? obj)) (eq? symbol (car obj))))

(define (grovel-includes)
  (let ((includes
         (tree-fold (lambda (x includes)
                      (if (and (list-with-head? 'include x)
                               (every string? (cdr x)))
                          (append includes (cdr x))
                          includes))
                    '()
                    (read))))
    (list-delete-neighbor-dups string=? (list-sort string<? includes))))

(define (import-set-library-name set)
  (if (not (list? set)) '()
      (case (car set)
        ((except only prefix rename)
         (import-set-library-name (list-ref set 1)))
        (else
         set))))

(define (grovel-imports)
  (let ((imports
         (tree-fold (lambda (x includes)
                      (if (list-with-head? 'import x)
                          (append includes
                                  (map import-set-library-name (cdr x)))
                          includes))
                    '()
                    (read))))
    (list-delete-neighbor-dups equal? (list-sort library-name<? imports))))

(define (library-imports lib-name)
  (let ((sld-file (library-name->sld lib-name)))
    (with-input-from-file (string-append live-root sld-file)
      grovel-imports)))

(define (disp . xs) (for-each display xs) (newline))

(define (library-includes lib-name)
  (let ((lib-dir (library-name->directory lib-name))
        (sld-file (library-name->sld lib-name)))
    (map (lambda (file) (string-append lib-dir file))
         (with-input-from-file (string-append live-root sld-file)
           grovel-includes))))

(define (library-includes-except-srfi lib-name)
  (remove (lambda (file) (string-contains file "srfi-"))
          (library-includes lib-name)))

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
         (dependencies ,@chicken-eggs)
         (test-dependencies)
         (distribution-files
          "live.egg"
          "live.release-info"
          ,@(append-map
             (lambda (lib-name)
               (cons (library-name->sld lib-name)
                     (library-includes-except-srfi lib-name)))
             (append-map library-names/r7rs libraries)))
         (components
          ,@(append-map
             (lambda (lib)
               (map (lambda (lib-name)
                      `(extension
                        ,(library-name->chicken lib-name)
                        (source ,(library-name->sld lib-name))
                        (source-dependencies
                         ,@(library-includes-except-srfi lib-name))
                        (component-dependencies
                         ,@(map library-name->chicken
                                (filter (lambda (name)
                                          (list-with-head? 'live name))
                                        (library-imports lib-name))))
                        (csc-options "-R" "r7rs" "-X" "r7rs")))
                    (library-names/r7rs lib)))
             libraries)))))))

(define (main)
  (write-chicken-5-egg-file))

(main)
