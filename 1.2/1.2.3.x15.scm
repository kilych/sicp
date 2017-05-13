(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (<= (abs angle) 0.1)
      angle
      (p (sine (/ angle 3)))))

;; a. Number of calls of procedure p for (sine 12.15) is 5.
;;
;; Substitution:
;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p 0.05)))))
;; and so on..
;;
;; Note:
;; trace REPL command shows that first call of p is (p 0.049..96) instead
;; of (p 0.05). Where is precision? Answer is (/ 0.15 3) returns
;; 0.049..96 instead of 0.05.


;; b. Linear growth of "time" and memory.


;; Optional
(define (profile-sine count limit)
  (sine 12.15)
  (if (= count limit)
      0
      (prof (+ count 1) limit)))
