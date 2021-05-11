(define (texinfo-read-file-header)
  (let ((header "\\input texinfo"))
    (let ((line (read-line)))
      (when (or (eof-object? line)
                (not (string=? header (string-trim-right line))))
        (error "Missing texinfo header" header)))))

(define (texinfo-read-document)
  (parameterize ((tex-escape-char #\@))
    (texinfo-read-file-header)
    (tex-read-document)))
