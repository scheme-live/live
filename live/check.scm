(library (live check)
  (export pk check check-raise check-skip check-values run-one-check main)

  (import (chezscheme))

  (include "check/body.scm"))
