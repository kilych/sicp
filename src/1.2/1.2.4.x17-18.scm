;;; Multiplication

;; x17
(define (multi a b)
  (if (= b 0)
      0
      (+ a (multi a (- b 1)))))

(define (my-even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((my-even? b) (double (fast-multi a
                                          (halve b))))
        (else (+ a (fast-multi a (- b 1))))))


;; x18
;; a*b+s is invariant
(define (fast-multi-iter x y)
  (define (iter a b s)
    (cond ((= b 0) s)
          ((my-even? b) (iter (double a)
                              (halve b)
                              s))
          (else (iter a (- b 1) (+ s a)))))
  (iter x y 0))
