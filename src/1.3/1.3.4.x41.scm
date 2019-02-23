(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

;; Substitution:
;; (((double (double double)) inc) 5)
;; (((double (lambda (x) (double (double x)))) inc) 5)
;; (((lambda (x) (double (double (double (double x))))) inc) 5)
;; ((lambda (x) (inc (inc (inc ... [16 times] ...  x)))) 5)
;; 21
