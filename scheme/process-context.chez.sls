(library (scheme process-context)
  (export get-environment-variable)
  (import (chezscheme))

  (define get-environment-variable getenv))
