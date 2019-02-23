(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (my-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (avg-of-two guess (/ x guess)))
  (define (sqrt-iter guess)
    (new-if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (square x) (* x x))

(define (avg-of-two x y)
  (/ (+ x y) 2))

;; My guess: "stack overflow" because new-if is procedure and Scheme
;; evaluates all operands of it anyway.
