(define (my-sqrt x)
  (define (improve guess)
    (avg-of-two guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))


(define (my-sqrt2 x)
  (define (improve guess)
    (avg-of-two guess (/ x guess)))

  (define (good-enough? guess)
    (< (/ (abs (- guess (improve guess))) guess) 0.01))

  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))


(define (square x) (* x x))

(define (avg-of-two x y)
  (/ (+ x y) 2))

;; Try (my-sqrt2 0). It doesn't work, but (my-sqrt 0) works.
