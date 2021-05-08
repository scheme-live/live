(define-library (live text html read)
  (export make-html-parser html->sxml html-strip)
  (import (scheme base) (scheme char) (scheme cxr) (scheme write))
  (include "read/chibi-html-parser.scm"))
