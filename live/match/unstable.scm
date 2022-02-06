;; Copyright (C) Felix Thibault (2020).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (live match unstable)
  (export
   ;; (chibi match) forms
   match match-lambda match-lambda* match-let match-letrec match-let*)
   ;; auxiliary syntax
   ;; ___ **1 =.. *.. *** ? $ struct object get!)
   ;; (import (only (live match auxiliary-syntax) ___ **1 =.. *.. *** ? $ struct object get!))
   (cond-expand
    (chibi
     (import (chibi)))
    (gauche
     (import (only (gauche base) is-a? slot-definition-name class-slots)
	     (scheme base)
	     (rename (gauche base)
		     (slot-ref gb-slot-ref)
		     (slot-set! gb-slot-set!)))
     (begin
       (define-syntax slot-ref
	 (syntax-rules ()
	   ((_ class inst n)
	    (if (integer? n)
		(gb-slot-ref inst
			     (list-ref (map slot-definition-name
					    (class-slots class))
				       n))
		(gb-slot-ref inst n)))))
       (define-syntax slot-set!
	 (syntax-rules ()
	   ((_ class inst n value)
	    (if (integer? n)
		(gb-slot-set! inst
			      (list-ref (map slot-definition-name
					     (class-slots class))
					n)
			      value)
		(gb-slot-set! inst n value)))))))
    ((or larceny (library (srfi 99)))
     (import (scheme base)
	     (srfi 99 records))
     (begin
       (define-syntax is-a?
	 (syntax-rules ()
	   ((_ rec rtd)
	    (and (rtd? rtd)
		 ((rtd-predicate rtd) rec)))))
       (define-syntax slot-ref
	 (syntax-rules ()
	   ((_ rtd rec n)
	    (if (integer? n)
		((rtd-accessor rtd (vector-ref (rtd-all-field-names rtd) n)) rec)
		((rtd-accessor rtd n) rec)))))
       (define-syntax slot-set!
	 (syntax-rules ()
	   ((_ rtd rec n value)
	    (if (integer? n)
		((rtd-mutator rtd (vector-ref (rtd-all-field-names rtd) n)) rec value)
		((rtd-mutator rtd n) rec value)))))))
    (unsyntax
     (import (except (scheme base) define-record-type)
	     (rnrs records syntactic (6))
	     (rnrs records procedural (6))
	     (rnrs records inspection (6)))
     (begin
       (define-syntax is-a?
	 (syntax-rules ()
	   ((_ rec rtd)
	    ((record-predicate (record-type-descriptor rtd)) rec))))
       (define-syntax slot-ref
	 (syntax-rules ()
	   ((_ rtd rec n)
	    (let ((rtd (record-type-descriptor rtd)))
	      (if (integer? n)
		  ((record-accessor rtd n) rec)
		  ((record-accessor rtd (name->idx rtd n)) rec))))))
       (define-syntax slot-set!
	 (syntax-rules ()
	   ((_ rtd rec n value)
	    (let ((rtd (record-type-descriptor rtd)))
	      (if (integer? n)
		  ((record-mutator rtd n) rec value)
		  ((record-mutator rtd (name->idx rtd n)) rec value))))))
       (define-syntax name->idx
	 (syntax-rules ()
	   ((_ rtd n)
	    (let* ((names (record-type-field-names rtd))
		   (len (vector-length names)))
	      (let lp ((i 0))
		(cond
		 ((> i len) (error "name not in record" n))
		 ((eq? n (vector-ref names i)) i)
		 (else (lp (+ i 1 )))))))))))
    (else
     (import (scheme base))))
   (include  "unstable/body.scm"))
