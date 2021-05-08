(define-library (live generator)
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator)
  (export gcons* gappend gcombine gfilter gremove
          gtake gdrop gtake-while gdrop-while
          gflatten ggroup gmerge gmap gstate-filter
          gdelete gdelete-neighbor-dups gindex gselect gfork)
  (export generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-map->list generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold)
  (export make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator)
  (export iterator? iterator-peek iterator-next!
          generator->iterator character-port->iterator iterator->generator)

  (import (scheme base))

  (cond-expand
   (kawa
    (import (scheme case-lambda)
            (schemepunk syntax)
            (only (live list) any)
            (only (kawa base) define-simple-class runnable try-catch this invoke-special)
            (class java.lang Runnable Thread InterruptedException)
            (class java.util.concurrent SynchronousQueue)
            (class gnu.mapping Procedure Procedure0))
    ;; Kawa doesn't support call/cc, but we can still implement
    ;; make-coroutine-generator with threads!
    (include "generator/schemepunk.kawa.scm")
    (include "generator/srfi-158.scm"))
   ((and (not chicken) (library (srfi 158)))
    (import (srfi 158)))
   ((and gerbil (library (std srfi 158)))
    (import (std srfi 158)))
   (else
    (import (scheme case-lambda)
            (only (live list) any))
    (include "generator/srfi-158.scm")))

  (include "generator/schemepunk.scm"))
