;; Snarfed from Kooda's geminid.
(define gemini-code-alist
  '((input                       . 10)
    (sensitive-input             . 11)
    (success                     . 20)
    (redirect                    . 30)
    (redirect-temporary          . 30)
    (redirect-permanent          . 31)
    (temporary-failure           . 40)
    (server-unavailable          . 41)
    (cgi-error                   . 42)
    (proxy-error                 . 43)
    (slow-down                   . 44)
    (permanent-failure           . 50)
    (not-found                   . 51)
    (gone                        . 52)
    (proxy-request-refused       . 53)
    (bad-request                 . 59)
    (client-certificate-required . 60)
    (certificate-not-authorised  . 61)
    (certificate-not-valid       . 62)))

(define (rassv key alist)
  (cond ((null? alist) #f)
        ((eqv? key (cdar alist)) (car alist))
        (else (rassv key (cddr alist)))))

(define (gemini-symbol->code symbol)
  (let ((entry (assq symbol gemini-code-alist)))
    (and entry (cdr entry))))

(define (gemini-code->symbol code)
  (let ((entry (rassv code gemini-code-alist)))
    (and entry (cdr entry))))

(define-record-type gemini-respose
  (make-gemini-response code meta port)
  gemini-response?
  (code gemini-response-code)
  (meta gemini-response-meta)
  (port gemini-response-port))

(define (gemini-response-first-digit response)
  (truncate-quotient (gemini-response-code response) 10))

(define (gemini-response-second-digit response)
  (truncate-remainder (gemini-response-code response) 10))

(define (gemini-response-success? response)
  (= 2 (gemini-response-first-digit response)))

(define (gemini-response-redirect? response)
  (= 3 (gemini-response-first-digit response)))

(define (gemini-response-raise response)
  (and (not (gemini-response-success? response))
       (raise (make-gemini-error response))))

(define (gemini-response-read-bytevector-all response)
  (let ((port (gemini-response-port response)))
    (let loop ((whole (bytevector)))
      (let ((part (read-bytevector 10000 port)))
        (if (eof-object? part) whole
            (loop (bytevector-append whole part)))))))

(define (gemini-response-read-string-all response)
  (utf8->string (gemini-response-read-bytevector-all response)))

(define (malformed-first-line line)
  (error "Malformed first line" line))

(define (read-cr-lf-terminated-line port)
  (let loop ((line ""))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (malformed-first-line line)
          (if (char=? #\return char)
              (let ((char (read-char port)))
                (if (char=? #\newline char)
                    line
                    (malformed-first-line line)))
              (loop (string-append line (string char))))))))
