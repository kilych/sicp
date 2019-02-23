(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y) (/ (+ x y) 2))

(define (fixed-point f guess)
  (let ((next (f guess)))
    ;; (display next)
    ;; (newline)
    (if (close-enough? guess next 0.00001)
        next
        (fixed-point f next))))

(define (close-enough? x y tolerance)
  (< (abs (- y x)) tolerance))


(define (my-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))


(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (square x) (* x x))


(define (fixed-point-of-transform transform f guess)
  (fixed-point (transform f) guess))



;;; Newton's method

(define (newton-method f guess)
  (fixed-point-of-transform newton-transform f guess))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (f (+ x dx)) (f x))
                   dx))))


(define (my-sqrt2 x)
  (newton-method (lambda (y) (- x (square y))) 1.0))



;;; x40

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))
