(define-library (live write)
  (export disp
          displayed
          displayln
          edisp
          writeln
          written)
  (import (scheme base)
          (scheme write)
          (live string))
  (include "write/live.scm"))
