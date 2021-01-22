(define-library (live hash-table)
  (export
   alist->hash-table
   hash-table
   hash-table-clear!
   hash-table-contains?
   hash-table-count
   hash-table-delete!
   hash-table-empty?
   hash-table-entries
   hash-table-find
   hash-table-fold
   hash-table-for-each
   hash-table-intern!
   hash-table-keys
   hash-table-map
   hash-table-map!
   hash-table-map->list
   hash-table-mutable?
   hash-table-pop!
   hash-table-prune!
   hash-table-ref
   hash-table-set!
   hash-table-size
   hash-table-unfold
   hash-table-update!
   hash-table-update!/default
   hash-table-values
   hash-table=?
   hash-table?
   make-hash-table
   )
  (import (scheme base))
  (cond-expand
    ((library (srfi 125))
     (import  (srfi 125)))
    ((library (srfi 69))
     (import  (srfi 69)))))

;; The following procedures are marked as deprecated in SRFI 125,
;; hence are not exported from this library.
;;
;; hash-table-exists?
;; hash-table-merge!
;; hash-table-walk
