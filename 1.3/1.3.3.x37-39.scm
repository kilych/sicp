;;; x37

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; k=11:
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) returns 0.6180..

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))



;;; x38

(define (e-number k)
  (define (n i) 1.0)
  (define (d i)
    (cond ((= i 2) 2.0)
          ((= (remainder i 3) 2) (- i 1.0))
          (else 1.0)))
  (+ 2 (cont-frac-iter n d k)))



;;; x39

(define (tan-cf x k)
  (define (n i) (if (= i 1)
                    x
                    (- (* x x))))
  (define (d i) (- (* 2 i) 1.0))
  (cont-frac-iter n d k))
