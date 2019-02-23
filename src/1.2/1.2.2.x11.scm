(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))


(define (f-iter n)
  (define (iter count a b c)
    (cond ((< n 3) n)
          ((= count n) c)
          (else (iter (+ count 1) b c (+ c a b)))))
  (iter 3 1 2 3))
