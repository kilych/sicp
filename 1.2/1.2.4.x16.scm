;;    n
;; a*b  is invariant quantity. Process runs step by step, but
;; the invariant doesn't mutate.


(define (my-fast-expt-iter base exponent)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((my-even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter base exponent 1))

(define (square x) (* x x))

(define (my-even? n)
  (= (remainder n 2) 0))
