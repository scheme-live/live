;; If object is...
;; * non-null and non-pair  returns 0; object
;; * the empty list         returns 0; ()
;; * a proper list          returns length; ()
;; * a dotted list          returns length; cdr of last pair
;; * a circular list        returns length before cycle; first pair of cycle
;;
;; The precise meaning of "length" is "number of pairs". Since list
;; elements are stored in the "car" of each pair, the number of pairs
;; is equal to the number of elements.
;;
;; <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare>
;;
(define (length-tail object)
  (let detect-cycle ((len 0) (slow object) (fast object))
    (cond ((not (pair? fast))
           (values (* 2 len) fast))
          ((not (pair? (cdr fast)))
           (values (+ 1 (* 2 len)) (cdr fast)))
          ((not (and (eq? slow fast) (> len 0)))
           (detect-cycle (+ 1 len) (cdr slow) (cdr (cdr fast))))
          (else
           (let find-cycle-start ((len 0) (slow object) (fast fast))
             (if (eq? slow fast) (values len slow)
                 (find-cycle-start (+ len 1) (cdr slow) (cdr fast))))))))

(define (proper-list? object)
  (let-values (((_ tail) (length-tail object)))
    (null? tail)))

(define (circular-list? object)
  (let-values (((_ tail) (length-tail object)))
    (pair? tail)))

(define (dotted-list? object)
  (let-values (((len tail) (length-tail object)))
    (not (or (= len 0) (null? tail) (pair? tail)))))

(define (map/odd f xs)
  (let loop ((acc '()) (xs xs) (odd? #f))
    (if (null? xs) (reverse acc)
        (loop (cons (f (car xs) odd?)
                    acc)
              (cdr xs)
              (not odd?)))))

(define (cons-right x xs)
  (append xs (list x)))

(define (last-index xs)
  (if (null? xs) #f (fx- (length xs) 1)))
