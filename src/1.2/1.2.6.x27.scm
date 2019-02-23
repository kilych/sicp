(define carmichael-numbers '(561
                             1105
                             1729
                             2465
                             2821
                             6601))

(define (carmichael-number? n)
  (and (not (prime? n))
       (fermat-test-all n)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (fermat-test-all n)
  (define (try-it a)
    (cond ((< a 1) #t)
          ((not (= a (expmod a n n))) #f)
          (else (try-it (- a 1)))))
  (try-it (- n 1)))

;; Odd clause:
;; (x1*x2) mod m = (x1*(k2*m+r2)) mod m = (x1*r2) mod m
;; Even clause:
;; (x1*x2) mod m = (r1*r2) mod m, where r1 = x1 mod m, r2 = x2 mod m
;; We can use just fast-expt and remainder but it isn't so comfortably
;; for big exponent.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
