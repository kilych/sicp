;; https://mitpress.mit.edu/sicp/code/ch3support.scm
;; For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 1)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((zero? trials-remaining) (/ trials-passed trials))
          ((experiment) (iter (1- trials-remaining) (1+ trials-passed)))
          (else (iter (1- trials-remaining) trials-passed))))
  (iter trials 0))


;;; Without rand
(define (estimate-pi2 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (let ((experiment (lambda () (= (gcd x1 x2) 1))))
          (cond ((zero? trials-remaining) (/ trials-passed trials))
                ((experiment)
                 (iter (1- trials-remaining) (1+ trials-passed) x2))
                (else
                 (iter (1- trials-remaining) trials-passed x2)))))))
  (iter trials 0 initial-x))


;;; x3.5
;; we assume that x2 >= x1 and y2 >= y1:
(define (estimate-integral pred x1 y1 x2 y2 trials)
  (define (in-shape-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (pred x y)))
  (let ((s (* (- x2 x1) (- y2 y1))))
    (* s (monte-carlo trials in-shape-test))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (zero-centered-unit-disc-pred x y)
  (disc-pred 0 0 1 x y))

(define (disc-pred center-x center-y radius x y)
  (>= (square radius)
      (+ (square (- x center-x)) (square (- y center-y)))))

(define (zero-centered-unit-square-pred x y)
  (square-pred 0 0 1 x y))

(define (square-pred center-x center-y side x y)
  (let ((half-side (/ side 2)))
    (and (<= (- center-x half-side) x)
         (>= (+ center-x half-side) x)
         (<= (- center-y half-side) y)
         (>= (+ center-y half-side) y))))

(define (square x) (* x x))

;; for rectangle that contains our shape smaller is better:
;; the frequency is closer to the probability
(define estimated-pi
  (estimate-integral zero-centered-unit-disc-pred -1 -1 1.0 1.0 1000))

(define estimated-unit-square
  (estimate-integral zero-centered-unit-square-pred -1 -1 1.0 1.0 1000))


;;; x3.6
;; my "anti-pattern" -
;; x isn't local state variable:
(define (rand2-bad message)
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-x) (set! x new-x))
    (cond ((eq? message 'generate) (generate))
          ((eq? message 'reset) reset)
          (else "Unknown method"))))

;; right implementation
(define rand2
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-x) (set! x new-x))
    (define (dispatch message)
      (cond ((eq? message 'generate) (generate))
            ((eq? message 'reset) reset)
            (else "Unknown method")))
    dispatch))
