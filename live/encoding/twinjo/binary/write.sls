;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC

(library (twinjo binary write)
  (export twinjo-binary-write)
  (import (rnrs)
          (only (chezscheme) integer-length)
          (srfi private include))
  (include/resolve () "write.scm"))
