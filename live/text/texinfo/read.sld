(define-library (live text texinfo read)
  (export texinfo-read-document)
  (import (scheme base)
          (live string)
          (live text tex read))
  (begin

    (define block-commands
      '(
        deffn
        defvr
        enumerate
        example
        format
        ifinfo
        ifnottex
        iftex
        ignore
        itemize
        macro
        menu
        multitable
        smallexample
        table
        titlepage)))

  (include "read/live.scm"))
