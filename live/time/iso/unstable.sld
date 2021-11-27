;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: MIT

;; https://en.wikipedia.org/wiki/ISO_8601

(define-library (live time iso unstable)
  (export parse-iso-duration)
  (import (scheme base)
          (scheme write)
          (live number unstable))
  (begin

    (define null-list? null?)
    (define first car)
    (define rest cdr)

    (define (acons key val alist)
      (cons (cons key val) alist))

    (define (split-numbers-letters str)
      (define (ascii-numeric? char) (char<=? #\0 char #\9))
      (let outer ((b 0) (parts '()))
        (if (= b (string-length str))
            (reverse parts)
            (let ((char (string-ref str b)))
              (if (ascii-numeric? char)
                  (let inner ((a b) (b b))
                    (if (and (< b (string-length str))
                             (ascii-numeric? (string-ref str b)))
                        (inner a (+ b 1))
                        (outer b (cons (string->number (string-copy str a b))
                                       parts))))
                  (outer (+ b 1) (cons char parts)))))))

    (define (parse-number-letter parts)
      (if (not (natural? (first parts)))
          (error "Natural number expected" (first parts))
          (let ((number (first parts))
                (letter (and (pair? (rest parts))
                             (let ((char (first (rest parts))))
                               (and (char? char)
                                    char)))))
            (if letter
                (values number letter (rest (rest parts)))
                (error "Letter expected" (rest parts))))))

    (define (parse-iso-duration-time parts result)
      (let loop ((parts parts) (result result))
        (if (null? parts) (reverse result)
            (let-values (((number letter parts)
                          (parse-number-letter parts)))
              (case letter
                ((#\H) (loop parts (acons 'hour number result)))
                ((#\M) (loop parts (acons 'minute number result)))
                ((#\S) (loop parts (acons 'second number result)))
                (else (error "Unknown letter" letter)))))))

    (define (parse-iso-duration parts)
      (let loop ((parts parts) (result '()))
        (cond ((null? parts)
               (reverse result))
              ((equal? #\T (first parts))
               (parse-iso-duration-time (rest parts) result))
              (else
               (let-values (((number letter parts)
                             (parse-number-letter parts)))
                 (case letter
                   ((#\W) (if (and (null? result) (null-list? parts))
                              (acons 'week number result)
                              (error "Week must be the only part")))
                   ((#\Y) (loop parts (acons 'year number result)))
                   ((#\M) (loop parts (acons 'month number result)))
                   ((#\D) (loop parts (acons 'day number result)))
                   (else (error "Unknown letter" letter))))))))))
