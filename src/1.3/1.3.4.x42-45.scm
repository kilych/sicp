;;; x42

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

((compose square inc) 6)
;; => 49



;;; x43

(define (repeated f n)
  (if (<= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; ((repeated square 3) 5)


(define (repeated-iter f n)
  (define (iter accum-f n)
    (if (<= n 0)
        accum-f
        (iter (compose f accum-f) (- n 1))))
  (iter identity n))

;; ((repeated-iter square 1) 5)



;;; x44

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (avg-of-3 (f (- x dx))
                          (f x)
                          (f (+ x dx))))))

(define (avg-of-3 x y z)
  (/ (+ x y z)
     3))


(define (n-fold-smooth f n)
  ((repeated-iter smooth n) f))



(define (test-proc f left-limit right-limit number-of-points)
  (define (create-list term list counter)
    (define shift (/ (- right-limit left-limit)
                     (- number-of-points 1)))
    (if (= counter 0)
        list
        (create-list (- term shift)
                     (cons term list)
                     (- counter 1))))
  (map f (create-list right-limit '() number-of-points)))

;; Example: (test-proc (n-fold-smooth cos 10) -1.5 1.5 7)



;;; x45

(define (4-root-bad x)
  (fixed-point (average-damp (lambda (y) (/ x (cube y)))) 1.0))

(define (4-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y)))))
               1.0))


;; It works, perhaps, not optimal. For example (n-root-test x 7 1)
;; works, but (floor (log2 7)) = 2 > 1.
(define (n-root x n)
  (n-root-test x n (floor (log2 n))))

(define (log2 x) (/ (log x) (log 2)))


(define (n-root-test x n damp-counter)
  (fixed-point-of-transform (repeated-iter average-damp damp-counter)
                            (lambda (y) (/ x (expt y (- n 1))))
                            1.0))


(define (cube x) (* x x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))


(define (fixed-point-of-transform transform f guess)
  (fixed-point (transform f) guess))

(define (fixed-point f guess)
  (let ((tolerance 0.00001)
        (next (f guess)))
    (if (close-enough? guess next tolerance)
        next
        (fixed-point f next))))

(define (close-enough? x y tolerance)
  (< (abs (- y x)) tolerance))



;;; Optional

(define (arbitrary-root x n)
  (define (iter f n)
    (cond ((= n 1) (fixed-point (average-damp f) 1.0))
          ((even? n) (iter (lambda (y) (my-sqrt (f y))) (/ n 2)))
          (else (iter (lambda (y) (/ (f y) y)) (- n 1)))))
  (iter (lambda (y) x) n))

(define (my-sqrt x)
  (fixed-point-of-transform average-damp
                            (lambda (y) (/ x y))
                            1.0))


;; Logarithmic computation procedure for arbitrary iterative
;; transformation (with procedure accumulation like in arbitrary-root).
