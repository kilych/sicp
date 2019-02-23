;; Transformation T: a,b -> b,a+b
;; T^n: a,b -> fib(n-1)*a + fib(n)*b, fib(n)*a + fib(n+1)*b
;; T^n[0,1] = [fib(n),fib(n+1)]
;; T^(2n)[0,1] = T^(n+n)[0,1] = T^n[fib(n),fib(n+1)] =
;; [fib(n-1)*fib(n) + fib(n)*fib(n+1), fib(n)*fib(n) + fib(n+1)*fib(n+1)]

(define (fast-fib-iter n)
  (define (iter trans-term trans-next-term exp-of-trans term next-term)
    (define (trans-prev-term) (- trans-next-term trans-term))
    (cond ((= exp-of-trans 0) term)
          ((my-even? exp-of-trans) (iter (* trans-term
                                             (+ (trans-prev-term)
                                                trans-next-term))
                                          (+ (square trans-term)
                                             (square trans-next-term))
                                          (/ exp-of-trans 2)
                                          term
                                          next-term))
          (else (iter trans-term
                      trans-next-term
                      (- exp-of-trans 1)
                      (+ (* term
                            (trans-prev-term))
                         (* next-term
                            trans-term))
                      (+ (* term
                            trans-term)
                         (* next-term
                            trans-next-term))))))
  (iter 1 1 n 0 1))

(define (my-even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))


;; Optional.
;; Generalization of fast exponetiation and fast fibonacci for
;; arbitrary "iterative" transformation.
