(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;; (A 1 10) returns 2^10.
;;
;; Substitution:
;; (A 1 5)
;; (A 0 (A 1 4))
;; (A 0 (A 0 (A 1 3)))
;; (A 0 (A 0 (A 0 (A 1 2))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 0 (A 0 (A 0 (A 0 2))))
;; (A 0 (A 0 (A 0 4)))
;; (A 0 (A 0 8))
;; (A 0 16)
;; 32 = 2^5


;;                    2
;;                   2
;;                  2   16
;; (A 2 4) returns 2 = 2 = 65536.
;;
;;                     2
;;                    2
;;                   2
;;                  2   65536
;; (A 2 5) returns 2 = 2.
;;
;; Substitution:
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 4))
;; (A 1 16)
;; 65536


;; (A 3 3) returns 65536.
;;
;; Substitution:
;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 4)
;; 65536
;;
;; (A 3 4)
;; ...
;; (A 2 65536)


;; (define (f n) (A 0 n)) implements 2n.
;;
;;                                    n
;; (define (g n) (A 1 n)) implements 2  but that returns 0 instead of
;; 1, when n = 0.
;;
;;                                          -+
;;                                         2 |
;;                                       ..  |
;;                                      2    | - n "floors"
;;                                    ..     |
;; (define (h n) (A 2 n)) implements 2       |
;;                                          -+
;;
;; Not asked but not without interest:
;; (define (k n) (A 3 n)) implements ???


;; Optional.
;; Iterative implementation for Ackerman function.
