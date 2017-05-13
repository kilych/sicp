(define (cube-root x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (- (abs (cube guess)) (abs x))) 0.001))

  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))


(define (cube-root2 x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (/ (- (abs (improve guess)) (abs guess)) guess))
       0.1))

  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))


(define (square x) (* x x))

(define (cube x) (* x x x))

(define (improve x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

;; Try (cube-root2 0). It doesn't work, but (cube-root 0) works.
