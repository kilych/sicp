;; Substitution:
;; (count-change 11)
;; (cc 11 5)
;; (+ (cc 11 4)
;;    (cc -39 5))
;; (+ (+ (cc 11 3)
;;       (cc -14 4))
;;    0)
;; (+ (+ (+ (cc 11 2)
;;          (cc 1 3))
;;       0)
;;    0)
;; (+ (+ (+ (+ (cc 11 1)
;;             (cc 6 2))
;;          (+ (cc 1 2)
;;             (cc -10 3)))
;;       0)
;;    0)
;; (+ (+ (+ (+ 1
;;             (+ (cc 6 1)
;;                (cc 1 2)))
;;          (+ (+ (cc 1 1)
;;                (cc -4 2))
;;             0))
;;       0)
;;    0)
;; (+ (+ (+ (+ 1
;;             (+ 1
;;                (+ (cc 1 1)
;;                   (cc -4 2))))
;;          (+ (+ 1
;;                0)
;;             0))
;;       0)
;;    0)
;; (+ (+ (+ (+ 1
;;             (+ 1
;;                (+ 1
;;                   0)))
;;          (+ (+ 1
;;                0)
;;             0))
;;       0)
;;    0)

;; Exponential growth of "time" and linear of memory (like fibonacci) ???