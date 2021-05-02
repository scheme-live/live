;; Unfinished coroutines can hang around and leak memory.
;; The only reliable way to fix this is to add a finalize() method
;; that kills threads when the generator is garbage collected.
(define-simple-class CoroutineGenerator (Procedure0)
  (proc ::Procedure access: 'private)
  (thread ::Thread access: 'private)
  ((*init* (pr ::Procedure) (th ::Thread))
   (invoke-special Procedure0 (this) '*init*)
   (set! proc pr)
   (set! thread th))
  ((apply0) (proc))
  ((finalize) access: 'public
   (thread:interrupt)))

(define (make-coroutine-generator proc)
  (define queue (SynchronousQueue))
  (define done #f)
  (define thread
    (chain (λ ()
             (try-catch (begin (proc (cut queue:put <>))
                               (let loop ()
                                 (queue:put (eof-object))
                                 (loop)))
                        (ex InterruptedException #f)))
           (runnable _)
           (Thread _)))
  (thread:setDaemon #t)
  (thread:start)
  (CoroutineGenerator
   (λ () (queue:take))
   thread))
