(define (iterative-improve predicate improve)
  (lambda (guess)
    (define (iter guess)
      (let ((next (improve guess)))
        (if (predicate guess next)
            next
            (iter next))))
    (iter guess)))


(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) 1.0))


(define (fixed-point f guess)
  ((iterative-improve close-enough? f) 1.0))


(define (close-enough? x y)
  (< (abs (- y x)) 0.00001))

(define (average x y) (/ (+ x y) 2))
