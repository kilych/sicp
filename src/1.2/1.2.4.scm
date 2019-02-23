(define (my-expt base n)
  (if (= n 0)
      1
      (* base (my-expt base (- n 1)))))

(define (my-expt-iter base n)
  (define (iter count product)
    (if (= count n)
        product
        (iter (+ count 1) (* product base))))
  (iter 0 1))

(define (square x) (* x x))

(define (my-even? n)
  (= (remainder n 2) 0))

;; Less steps. Faster for big exponent, for example 1000.
(define (my-fast-expt base n)
  (cond ((= n 0) 1)
        ((my-even? n) (square (my-fast-expt base
                                            (/ n 2))))
        (else (* base (my-fast-expt base
                                    (- n 1))))))
