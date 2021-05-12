(define-library (live database sqlite)
  (export sql-open
          sql-close
          sql-do
          sql-do/row-id
          sql-get-one
          sql-get-all)
  (cond-expand (gambit
                (import (gambit))
                (pkg-config "sqlite3")
                (include "sqlite/live.gambit.scm"))))
