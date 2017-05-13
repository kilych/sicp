(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (iter accum result)
    (if (<= n accum)
        result
        (iter (+ accum 1) (* result (+ accum 1)))))
  (iter 1 1))
