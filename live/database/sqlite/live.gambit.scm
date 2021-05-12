(define (list-accumulator)
  (let ((xs '()))
    (lambda (x)
      (if (eof-object? x) (reverse xs) (set! xs (cons x xs))))))

(define (plist-for-each proc plist)
  (let loop ((tail plist))
    (cond ((and (pair? tail) (symbol? (car tail)) (pair? (cdr tail)))
           (proc (car tail) (cadr tail))
           (loop (cddr tail)))
          ((not (null? tail))
           (error "Not a valid property list:" plist)))))

;;

(c-declare "#include <string.h>")
(c-declare "#include <sqlite3.h>")

(c-define-type sqlite3 (pointer "sqlite3"))
(c-define-type sqlite3-stmt (pointer "sqlite3_stmt"))
(c-define-type sqlite3-value (pointer "sqlite3_value"))

(define SQLITE_BLOB    ((c-lambda () int "___return(SQLITE_BLOB);")))
(define SQLITE_DONE    ((c-lambda () int "___return(SQLITE_DONE);")))
(define SQLITE_FLOAT   ((c-lambda () int "___return(SQLITE_FLOAT);")))
(define SQLITE_INTEGER ((c-lambda () int "___return(SQLITE_INTEGER);")))
(define SQLITE_NULL    ((c-lambda () int "___return(SQLITE_NULL);")))
(define SQLITE_ROW     ((c-lambda () int "___return(SQLITE_ROW);")))
(define SQLITE_TEXT    ((c-lambda () int "___return(SQLITE_TEXT);")))

;;

(define %sqlite3-errstr
  (c-lambda (int) nonnull-UTF-8-string
    "sqlite3_errstr"))

(define %sqlite3-errmsg
  (c-lambda (sqlite3) nonnull-UTF-8-string
    "sqlite3_errmsg"))

(define %sqlite3-set-last-insert-rowid
  (c-lambda (sqlite3 int64) void
    "sqlite3_set_last_insert_rowid"))

(define %sqlite3-last-insert-rowid
  (c-lambda (sqlite3) int64
    "sqlite3_last_insert_rowid"))

(define %sqlite3-bind-parameter-index
  (c-lambda (sqlite3-stmt nonnull-UTF-8-string) int
    "sqlite3_bind_parameter_index"))

(define %sqlite3-bind-null
  (c-lambda (sqlite3-stmt int) int
    "sqlite3_bind_null"))

(define %sqlite3-bind-int64
  (c-lambda (sqlite3-stmt int int64) int
    "sqlite3_bind_int64"))

(define %sqlite3-bind-double
  (c-lambda (sqlite3-stmt int double) int
    "sqlite3_bind_double"))

(define %sqlite3-bind-text
  (c-lambda (sqlite3-stmt int nonnull-UTF-8-string) int
    "___return(sqlite3_bind_text(___arg1, ___arg2, ___arg3, -1,
       SQLITE_TRANSIENT));"))

(define (%sqlite3-bind-blob %stmt index u8vector)
  (unless (u8vector? u8vector)
    (error "Not a u8vector:" u8vector))
  ((c-lambda (sqlite3-stmt int scheme-object int) int
     "void *bytes = ___CAST(void *, ___BODY(___arg3));
      ___return(sqlite3_bind_blob(___arg1, ___arg2, bytes, ___arg4,
        SQLITE_TRANSIENT));")
   %stmt index u8vector (u8vector-length u8vector)))

(define %sqlite3-column-value
  (c-lambda (sqlite3-stmt int) sqlite3-value
    "sqlite3_column_value"))

(define %sqlite3-value-type
  (c-lambda (sqlite3-value) int
    "sqlite3_value_type"))

(define %sqlite3-value-int64
  (c-lambda (sqlite3-value) int64
    "sqlite3_value_int64"))

(define %sqlite3-value-double
  (c-lambda (sqlite3-value) double
    "sqlite3_value_double"))

(define %sqlite3-value-text
  (c-lambda (sqlite3-value) UTF-8-string
    "sqlite3_value_text"))

(define %sqlite3-finalize
  (c-lambda (sqlite3-stmt) int
    "sqlite3_finalize"))

(define %sqlite3-step
  (c-lambda (sqlite3-stmt) int
    "sqlite3_step"))

(define %sqlite3-column-count
  (c-lambda (sqlite3-stmt) int
    "sqlite3_column_count"))

(define %sqlite3-close
  (c-lambda (sqlite3) int
    "sqlite3_close"))

;;

(define %sqlite3-value-blob
  (c-lambda (sqlite3-value) scheme-object
    "const void *bytes = sqlite3_value_blob(___arg1);
     int nbytes = sqlite3_value_bytes(___arg1);
     ___SCMOBJ vec =
       ___EXT(___alloc_scmobj)(___PSTATE, ___sU8VECTOR, nbytes);
     if (___FIXNUMP(vec)) ___return(___FAL);
     ___U8 *elts = ___CAST(___U8 *, ___BODY(vec));
     memcpy(elts, bytes, nbytes);
     ___EXT(___release_scmobj)(vec);
     ___return(vec);"))

(define %sqlite3-prepare-v2
  (c-lambda (sqlite3 nonnull-UTF-8-string) scheme-object
    "___SCMOBJ scheme_stmt;
     ___SCMOBJ scheme_vec;
     ___SCMOBJ scheme_err;
     sqlite3_stmt *stmt;
     const char *tail;
     int err = sqlite3_prepare_v2(___arg1, ___arg2, -1, &stmt, &tail);
     scheme_err = ___NONNULLPOINTER_to_SCMOBJ(
       ___PSTATE, stmt, ___FAL, NULL, &scheme_stmt, ___RETURN_POS);
     // if (scheme_err != ___FIX(___NO_ERR)) ___return(scheme_err);
     scheme_vec = ___make_vector(___PSTATE, 3, ___FAL);
     ___VECTORSET(scheme_vec, ___FIX(0), ___FIX(err));
     ___VECTORSET(scheme_vec, ___FIX(1), scheme_stmt);
     ___VECTORSET(scheme_vec, ___FIX(2), (strlen(tail) ? ___TRU : ___FAL));
     ___EXT(___release_scmobj)(scheme_vec);
     ___return(scheme_vec);"))

(define %sqlite3-open
  (c-lambda (nonnull-UTF-8-string) scheme-object
    "___SCMOBJ scheme_db;
    ___SCMOBJ scheme_err;
    sqlite3 *db;
    int err = sqlite3_open(___arg1, &db);
    if (err) ___return(___FIX(err));
    scheme_err = ___NONNULLPOINTER_to_SCMOBJ(
      ___PSTATE, db, ___FAL, NULL, &scheme_db, ___RETURN_POS);
    if (scheme_err !=___FIX(___NO_ERR)) ___return(scheme_err);
    ___return(scheme_db);"))

;;

(define (raise-sqlite-error c-proc err)
  (if (= 0 err) #f
      (error "SQLite error:" c-proc (%sqlite3-errstr err))))

(define (raise-sqlite-db-error db c-proc err)
  (if (= 0 err) #f
      (error "SQLite error:" c-proc (%sqlite3-errmsg db))))

(define (internal-value-to-scheme value)
  (let ((type (%sqlite3-value-type value)))
    (cond ((= type SQLITE_NULL)    'null)
          ((= type SQLITE_BLOB)    (%sqlite3-value-blob   value))
          ((= type SQLITE_TEXT)    (%sqlite3-value-text   value))
          ((= type SQLITE_INTEGER) (%sqlite3-value-int64  value))
          ((= type SQLITE_FLOAT)   (%sqlite3-value-double value))
          (else (error "Got strange kind of value from SQLite")))))

(define (internal-get-row db %stmt)
  (let ((err (%sqlite3-step %stmt)))
    (cond ((= err SQLITE_DONE)
           (eof-object))
          ((= err SQLITE_ROW)
           (let loop ((n (%sqlite3-column-count %stmt)) (columns '()))
             (if (<= n 0) columns
                 (loop (- n 1)
                       (cons (internal-value-to-scheme
                              (%sqlite3-column-value %stmt (- n 1)))
                             columns)))))
          (else (raise-sqlite-db-error db 'sqlite3_step err)))))

(define (internal-finalize db %stmt)
  (raise-sqlite-db-error db 'sqlite3_finalize (%sqlite3-finalize %stmt)))

(define (internal-bind-parameter db %stmt i value)
  (raise-sqlite-db-error
   db
   'sqlite3_bind_*
   (cond ((eq? 'null value)
          (%sqlite3-bind-null %stmt i))
         ((string? value)
          (%sqlite3-bind-text %stmt i value))
         ((u8vector? value)
          (%sqlite3-bind-blob %stmt i value))
         ((and (integer? value) (exact-integer? value))
          (%sqlite3-bind-int64 %stmt i value))
         ((real? value)
          (%sqlite3-bind-double %stmt i (inexact value)))
         (else
          (error "Cannot represent value in SQLite:" value)))))

(define (internal-prepare db stmt)
  (let ((sql-string (if (string? stmt) stmt (car stmt)))
        (sql-params (if (string? stmt) '()  (cdr stmt))))
    (unless (string? sql-string)
      (error "SQL not a string:" sql-string))
    (let* ((retvals (%sqlite3-prepare-v2 db sql-string))
           (err     (vector-ref retvals 0))
           (%stmt   (vector-ref retvals 1))
           (tail    (vector-ref retvals 2)))
      (raise-sqlite-db-error db 'sqlite3_prepare_v2 err)
      (with-exception-catcher
       (lambda (err)
         (internal-finalize db %stmt)
         (raise err))
       (lambda ()
         (when tail
           (error "Cannot execute more than one SQL statement at once"))
         (plist-for-each
          (lambda (name value)
            (let ((i (%sqlite3-bind-parameter-index
                      %stmt (string-append "@" (symbol->string name)))))
              (when (<= i 0) (error "No such parameter:" name))
              (internal-bind-parameter db %stmt i value)))
          sql-params)
         %stmt)))))

;;

(define (sql-open dbname . params)
  (let ((db/err (%sqlite3-open dbname)))
    (if (number? db/err) (raise-sqlite-error 'sqlite3_open db/err) db/err)))

(define (sql-close db)
  (raise-sqlite-db-error db 'sqlite3_close (%sqlite3-close db)))

(define (sql-do db stmt)
  (let ((%stmt (internal-prepare db stmt)))
    (let ((err (%sqlite3-step %stmt)))
      (if (= err SQLITE_DONE)
          (internal-finalize db %stmt)
          (raise-sqlite-db-error db 'sqlite3_step err))
      #f)))

(define (sql-do/row-id db stmt)
  (define no-such-row-id 0)
  (%sqlite3-set-last-insert-rowid db no-such-row-id)
  (sql-do db stmt)
  (let ((row-id (%sqlite3-last-insert-rowid db)))
    (if (= no-such-row-id row-id)
        (error "Cannot get row-id after sql-do/row-id")
        row-id)))

(define (sql-get-one db stmt #!optional mapfun)
  (let* ((mapfun (or mapfun values))
         (%stmt (internal-prepare db stmt))
         (row (internal-get-row db %stmt)))
    (when (eof-object? row)
      (error "Got no rows"))
    (unless (eof-object? (internal-get-row db %stmt))
      (error "Got more than one row"))
    (internal-finalize db %stmt)
    (apply mapfun row)))

(define (sql-get-all db stmt #!optional mapfun accumulator)
  (let* ((mapfun (or mapfun vector))
         (accumulator (or accumulator (list-accumulator)))
         (%stmt (internal-prepare db stmt)))
    (let loop ()
      (let ((row (internal-get-row db %stmt)))
        (cond ((eof-object? row)
               (internal-finalize db %stmt)
               (accumulator (eof-object)))
              (else
               (accumulator (apply mapfun row))
               (loop)))))))
