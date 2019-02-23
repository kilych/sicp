(define (search f neg-point pos-point)
  (define midpoint (average neg-point pos-point))
  (let ((test-value (f midpoint)))
    (cond ((close-enough? neg-point pos-point 0.001) midpoint)
          ((positive? test-value) (search f neg-point midpoint))
          ((negative? test-value) (search f midpoint pos-point))
          (else midpoint))))

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x1 x2 tolerance)
  (< (abs (- x2 x1)) tolerance))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (positive? a-value) (negative? b-value)) (search f b a))
          ((zero? a-value) a)
          ((zero? b-value) b)
          (else (error "f(a) has the same sign as f(b) "
                       a-value
                       b-value)))))

;; (half-interval-method sin 2.0 4.0)
;; -> 3.14111328125

;; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
;; 1.89306640625


;; If f(x)=x then x - fixed point of f.
(define (fixed-point f guess)
  (let ((next (f guess)))
    (if (close-enough? guess next 0.00001)
        next
        (fixed-point f next))))

;; (fixed-point cos 1.0)
;; => 0.7390822985224023

;; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;; 1.2587315962971173


(define (bad-sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; Implementation with average damping.
;; y=x/y ~ y=(1/2)*(y+x/y)
(define (my-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))



;;; x35

;; Golden ratio phi is fixed point for transformation x -> 1 + 1/x
;; because phi=1+1/phi ~ phi/1=(phi+1)/phi.
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))



;;; x36

(define (fixed-point-show f guess)
  (let ((next (f guess)))
        (display next)
        (newline)
        (if (close-enough? guess next 0.00001)
            next
            (fixed-point-show f next))))

;; Finding solution for x^x = 1000:
(define (example)
  (fixed-point-show (lambda (x) (/ (log 1000) (log x))) 4.0))
;; 29 iterations

;; Finding solution for x^x = 1000 with average damping:
(define (example-avg-damp)
  (fixed-point-show (lambda (x) (average x (/ (log 1000) (log x)))) 4.0))
;; 7 iterations
