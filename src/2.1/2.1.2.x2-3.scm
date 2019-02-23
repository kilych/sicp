;;; x2

(define (midpoint-segment segment)
  (make-point (+ (x-pos (start-segment segment))
                 (/ (x-project-segment segment) 2))
              (+ (y-pos (start-segment segment))
                 (/ (y-project-segment segment) 2))))

(define (length-segment segment)
  (sqrt (+ (square (x-project-segment segment))
           (square (y-project-segment segment)))))

(define (x-project-segment segment)
  (abs (- (x-pos (end-segment segment))
          (x-pos (start-segment segment)))))

(define (y-project-segment segment)
  (abs (- (y-pos (end-segment segment))
          (y-pos (start-segment segment)))))

(define (make-segment point-one point-two)
  (cons point-one point-two))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (print-point point)
  (display "(")
  (display (x-pos point))
  (display ",")
  (display (y-pos point))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-pos point)
  (car point))

(define (y-pos point)
  (cdr point))



;;; x3

(define (area-rect rect)
  (* (car (lengths-rect rect))
     (cdr (lengths-rect rect))))

(define (perimeter-rect rect)
  (+ (* 2 (car (lengths-rect rect)))
     (* 2 (cdr (lengths-rect rect)))))


;; (define (make-rect segment perpendicular-length)
;;          (cons segment perpendicular-length))

;; (define (lengths-rect rect)
;;   (cons (length-segment (car rect)) (cdr rect)))


;; (cdr (cons 1 2))
;; => 2
;; (cdr (list 1 2))
;; => (2)
(define (make-rect orientation-angle
                   length
                   perpendicular-length)
  (cons orientation-angle
        (cons (abs length)
              (abs perpendicular-length))))

(define (lengths-rect rect)
  (cdr rect))



;;; General math stuff

(define (square x) (* x x))



;;; Examples

(define A (make-point -3 0.4))
(define B (make-point 2 4))
(define C (make-point 5 0))
(define D (make-point -2 -3.5))

(define AB (make-segment A B))
(define BC (make-segment B C))
(define CD (make-segment C D))
(define DA (make-segment D A))
(define CA (make-segment C A))
(define BD (make-segment B D))
