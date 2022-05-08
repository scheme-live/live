(library (scheme file)
  (export call-with-input-file open-binary-input-file)
  (import (rename (chezscheme) (open-file-input-port open-binary-input-file))))
