;; Copyright (c) 2004 James Bailey (dgym.REMOVE_THIS.bailey@gmail.com).
;; Copyright (c) 2009 Jim Ursetto.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; base64 routines for bigloo, apart from the module info, bit routines, "when"
;; and fixed division "/fx" it should be slightly portable

;; Ported to CHICKEN by felix
;; Rewritten for CHICKEN by Jim Ursetto.  Notes:
;;  Local anonymous functions (bits-at) are not inlined; use define-inline. (removed, because define-inline does not respect local scoping)
;;  Toplevel tables moved to lexical scope.
;;  Encode algorithm moves the test for 1 or 2 remaining bytes out
;;  of the main loop; generates -significantly- better code under Chicken.
;;  Decode algorithm rewritten as state machine; invalid input is
;;  silently skipped.
;;  Compiling with -unsafe is HIGHLY recommended, and gains more benefit
;;  as your inner loop gets tighter.
;;  The optimized variants are almost on par with pure C.
;;  Encoding and decoding can now operate on ports.

(declare (fixnum))

(module base64
  (base64-encode base64-decode base64-line-breaks)

  (import scheme chicken.base chicken.bitwise chicken.fixnum
          (only chicken.io read-string!)
          (only srfi-13 string-concatenate-reverse))

;; If base64-line-breaks is true, a CRLF is inserted every
;; 76 output chars (57 input chars) and at the end of the last
;; line, if it was partial (between 1 and 75 output chars).
(define base64-line-breaks (make-parameter #f))

;; Optimized string->string implementation
(define (base64-encode/string->string str)
  (define (bits-at idx)
    (char->integer (string-ref str idx)))
  (define (b64->char n)
    (define enc-table
      '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
         #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
         #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
         #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
         #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))
    (vector-ref enc-table (bitwise-and n 63)))

  (define (encode-tail out i o r)
    ;; Handle last 1 or 2 bytes
    (case r
      ((0) o)
      ((1)
       (let ((n (arithmetic-shift (bits-at i) 16)))
         (string-set! out o (b64->char (arithmetic-shift n -18)))
         (string-set! out (+ o 1) (b64->char (arithmetic-shift n -12)))
         (+ o 4)))
      ((2)
       (let ((n (bitwise-ior (arithmetic-shift (bits-at i) 16)
                             (arithmetic-shift (bits-at (+ i 1)) 8))))
         (string-set! out o (b64->char (arithmetic-shift n -18)))
         (string-set! out (+ o 1) (b64->char (arithmetic-shift n -12)))
         (string-set! out (+ o 2) (b64->char (arithmetic-shift n -6)))
         (+ o 4)))))

  (##sys#check-string str 'base64-encode)
  (let ((l (string-length str)))
    (let* ((nobreak? (not (base64-line-breaks)))
           (outlen (* 4 (fx/ (+ l 2) 3)))
           (full-lines (fx/ l 57))
           (partial-line (not (= 0 (fxmod l 57))))
           (outlen (if nobreak?
                       outlen
                       (+ outlen (fx* 2 (+ full-lines
                                           (if partial-line 1 0))))))
           (out (make-string outlen #\=)))
      (let ((o
             (let loop ((i 0) (o 0) (r l) (c 1))
               (if (< r 3)
                   (encode-tail out i o r)
                   (let ((n (bitwise-ior (arithmetic-shift (bits-at i) 16)
                                         (arithmetic-shift (bits-at (+ i 1)) 8)
                                         (bits-at (+ i 2)))))
                     (string-set! out o       (b64->char (arithmetic-shift n -18)))
                     (string-set! out (+ o 1) (b64->char (arithmetic-shift n -12)))
                     (string-set! out (+ o 2) (b64->char (arithmetic-shift n -6)))
                     (string-set! out (+ o 3) (b64->char n))
                     (cond (nobreak?
                            (loop (+ i 3) (+ o 4) (- r 3) c))
                           ((< c 19)  ; 57/3 = 76/4 = 19
                            (loop (+ i 3) (+ o 4) (- r 3) (+ c 1)))
                           (else
                            (string-set! out (+ o 4) #\return)
                            (string-set! out (+ o 5) #\newline)
                            (loop (+ i 3) (+ o 6) (- r 3) 1)))
                     )))))
        (when (and (not nobreak?)
                   partial-line)
          (string-set! out o #\return)
          (string-set! out (+ o 1) #\newline))
        out))))

(define (base64-encode in #!optional out)
  (define (port-to-port in out)
    (let* ((buflen (* 57 60))
           (buf (make-string buflen)))
      (let lp ()
        (let ((n (read-string! buflen buf in)))
          (cond ((= n 0) out)
                (else
                 (display (base64-encode/string->string
                           (if (< n buflen) (substring buf 0 n) buf))
                          out)
                 (lp)))))))
  (define (port-to-string in)
    ;; easier on GC than (let ((out (open-output-string)))
    ;;                     (get-output-string (port-to-port in out)))
    (let* ((buflen (* 57 60))
           (buf (make-string buflen)))
      (let lp ((lines '()))
        (let ((n (read-string! buflen buf in)))
          (cond ((= n 0)
                 (string-concatenate-reverse lines))
                (else
                 (lp (cons (base64-encode/string->string
                            (if (< n buflen) (substring buf 0 n) buf))
                           lines))))))))
  (if (port? out)
      (if (string? in)
          (port-to-port (open-input-string in) out)
          (port-to-port in out))
      (if (string? in)
          (base64-encode/string->string in)
          (port-to-string in))))

;; (define (calc-dec-table)
;;    (let ((res (make-vector 256 -1)))
;;      (do ((i 0 (+ i 1)))
;;          ((>= i 64))
;;        (vector-set! res (char->integer (vector-ref enc-table i)) i))
;;      res))


;; Optimized string->string decoder implementation.  A bit faster than
;; the partial decoder--part of which is less garbage generation due
;; to a better string length guess in the best possible case--but the
;; partial decoder is more general.  So we will probably drop this.
;; It's not currently used.
(define (base64-decode/string->string str)
  (define (bits-at idx)
    (define dec-table
         '#(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 62 -1 -1 -1 63
            52 53 54 55 56 57 58 59 60 61 -1 -1 -1 -1 -1 -1
            -1  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
            15 16 17 18 19 20 21 22 23 24 25 -1 -1 -1 -1 -1
            -1 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
            41 42 43 44 45 46 47 48 49 50 51 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
    (vector-ref dec-table (char->integer (string-ref str idx))))
  (define (int->char n)
    (integer->char (bitwise-and n 255)))
  ;; Guess upper bound for string length--assumes no invalid characters
  ;; encountered, and checks the last two chars for validity
  ;; in strings of length 4n.
  (define (guess-out-length l)           ; assumes L > 0
    (let ((floored (fx* 4 (fx/ l 4))))
      (+ (fx* 3 (fx/ l 4))
         (cond ((not (= l floored)) 3)
               (else
                (if (= -1 (bits-at (- l 1)))
                    (if (= -1 (bits-at (- l 2))) -2 -1)
                    0))))))

  (##sys#check-string str 'base64-decode)
  (let ((l (string-length str)))
    (if (= l 0)
        str
        (let* ((outlen (guess-out-length l))  ; avoid substring if possible
               (out (make-string outlen))
               (o
                (let loop ((i 0) (o 0) (state 0) (n 0))
                  (if (>= i l)
                      o
                      (let ((b (bits-at i)))
                        (if (= -1 b)
                            (loop (+ i 1) o state n)
                            (case state
                              ((0) (loop (+ i 1) o 1 b))
                              ((1) (let ((n (bitwise-ior b (arithmetic-shift n 6))))
                                     (string-set! out o (int->char (arithmetic-shift n -4)))
                                     (loop (+ i 1) (+ o 1) 2 n)))
                              ((2) (let ((n (bitwise-ior b (arithmetic-shift n 6))))
                                     (string-set! out o (int->char (arithmetic-shift n -2)))
                                     (loop (+ i 1) (+ o 1) 3 n)))
                              (else (let ((n (bitwise-ior b (arithmetic-shift n 6))))
                                      (string-set! out o (int->char n))
                                      (loop (+ i 1) (+ o 1) 0 0))))))))))
          ;; Pull this out of the loop; otherwise the code is pessimized.
          (if (= outlen o)
              out
              (substring out 0 o))))))

(define (base64-decode in #!optional out)
  (define (port-to-port in out)
    (let* ((buflen 4096)
           (buf (make-string buflen))
           (st (vector 0 0 0 0)))
      (let lp ()
        (let ((n (read-string! buflen buf in)))
          (cond ((< n buflen)   ; works for ""
                 (display (base64-decode-partial (substring buf 0 n)
                                                 st #f)
                          out)
                 out)
                (else
                 (display (base64-decode-partial buf st #t)
                          out)
                 (lp)))))))
  (define (port-to-string in)
    (let* ((buflen 4096)
           (buf (make-string buflen))
           (st (vector 0 0 0 0)))
      (let lp ((lines '()))
        (let ((n (read-string! buflen buf in)))
          (cond ((< n buflen)
                 (string-concatenate-reverse
                  (cons (base64-decode-partial (substring buf 0 n) st #f)
                        lines)))
                (else
                 (lp (cons (base64-decode-partial buf st #t)
                           lines))))))))
  (if (port? out)
      (if (string? in)
          (port-to-port (open-input-string in) out)
          (port-to-port in out))
      (if (string? in)
;;           (base64-decode/string->string in)
          (let ((st (vector 0 0 0 0)))
            (base64-decode-partial in st #f))
          (port-to-string in))))

;; Incremental base64 decoder
;; Requires initial state vector st: #(state c1 c2 c3)
;; Returns: str; mutates state vector st when more?.
;; If a full 4 encoded characters are not available, AND there is
;; possibly more data, we cannot decode the remaining chars.  We must
;; retain up to 3 input characters along with the current
;; input state, so the decoder may be restarted where it left off.
(define (base64-decode-partial str st more?)
  (define (bits-at idx)
    (define dec-table
      '#(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 62 -1 -1 -1 63
            52 53 54 55 56 57 58 59 60 61 -1 -1 -1 -1 -1 -1
            -1  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
            15 16 17 18 19 20 21 22 23 24 25 -1 -1 -1 -1 -1
            -1 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
            41 42 43 44 45 46 47 48 49 50 51 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
            -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
    (vector-ref dec-table (char->integer (string-ref str idx))))
  (define (int->char n)
    (integer->char (bitwise-and n 255)))
  ;; Upper bound for string length--nothing fancy for partial reads.
  ;; But add state (# of chars pending) to input length.
  (define (guess-out-length len state)
    (let ((c (+ state len)))
      (if (= 0 (bitwise-and c 3))   ; (fxmod c 4)
          (fx* 3 (fx/ c 4))
          (fx* 3 (+ 1 (fx/ c 4))))))

  ;; When no MORE? data, write out the remaining chars.
  (define (decode-tail out o state c1 c2 c3)
    (case state
      ((0 1) o)
      ((2) (let ((n (bitwise-ior (arithmetic-shift c1 18)
                                 (arithmetic-shift c2 12))))
             (string-set! out o       (int->char (arithmetic-shift n -16)))
             (+ o 1)))
      ((3) (let ((n (bitwise-ior
                     (bitwise-ior (arithmetic-shift c1 18)
                                  (arithmetic-shift c2 12))
                     (arithmetic-shift c3 6))))
             (string-set! out o       (int->char (arithmetic-shift n -16)))
             (string-set! out (+ o 1) (int->char (arithmetic-shift n -8)))
             (+ o 2)))))

  ;; Finish up.  The state vector has already been updated unconditionally;
  ;; write the remaining chars into the buffer if we expect no more data.  Return
  ;; the buffer, truncating if necessary.
  (define (do-tail out o st)
    (let ((o (if more? o
                 (decode-tail out o
                              (vector-ref st 0)
                              (vector-ref st 1)
                              (vector-ref st 2)
                              (vector-ref st 3)))))
      (if (= o (string-length out))
          out
          (substring out 0 o))))

  (##sys#check-string str 'base64-decode)
  (let* ((len (string-length str))
         (state (vector-ref st 0))
         (outlen (guess-out-length len state))
         (out (make-string outlen)))
    (let ((o
           (let loop ((i 0) (o 0) (state state)
                      (c1 (vector-ref st 1))
                      (c2 (vector-ref st 2))
                      (c3 (vector-ref st 3)))
             (cond ((>= i len)
                    (vector-set! st 0 state)
                    (vector-set! st 1 c1)
                    (vector-set! st 2 c2)
                    (vector-set! st 3 c3)
                    o)
                   (else
                    (let ((c (bits-at i)))
                      (if (= -1 c)
                          (loop (+ i 1) o state c1 c2 c3)
                          (case state
                            ((0) (loop (+ i 1) o 1 c  c2 c3))
                            ((1) (loop (+ i 1) o 2 c1 c  c3))
                            ((2) (loop (+ i 1) o 3 c1 c2 c ))
                            (else
                             (let ((n (bitwise-ior
                                       (bitwise-ior (arithmetic-shift c1 18)
                                                    (arithmetic-shift c2 12))
                                       (bitwise-ior (arithmetic-shift c3 6)
                                                    c))))
                               (string-set! out o       (int->char (arithmetic-shift n -16)))
                               (string-set! out (+ o 1) (int->char (arithmetic-shift n -8)))
                               (string-set! out (+ o 2) (int->char n))
                               (loop (+ i 1) (+ o 3) 0 c1 c2 c3)))))))))))
      ;; Pull out of loop to avoid stack probe and interrupt check
      ;; causing > 2x slowdown.  decode-tail arguments must then
      ;; be pulled from the state vector.
      (do-tail out o st))))

)
