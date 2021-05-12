(define-library (minimal-sqlite)
  (export sql-open
          sql-close
          sql-do
          sql-do/row-id
          sql-get-one
          sql-get-all)
  (cond-expand (gambit
                (import (gambit))
                (pkg-config "sqlite3")
                (include "minimal-sqlite-gambit.scm"))))
