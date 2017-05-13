(define (cons p q)
  (* (expt 2 p) (expt 3 q)))

(define (car z)
  (define (accum z p)
    (if (even? z)
        (accum (/ z 2) (+ p 1))
        p))
  (accum z 0))

(define (cdr z)
  (define (accum z q)
    (if (= (remainder z 3) 0)
        (accum (/ z 3) (+ q 1))
        q))
  (accum z 0))

;; Optional: maybe O(log(n)) for number of steps?
