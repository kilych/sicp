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



;;; Fermat test

(define (fast-prime10? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; a < n => a mod n = a
(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random (- n 1)))))

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



;;; x21

;; (smallest-divisor 199)
;; => 199

;; (smallest-divisor 1999)
;; => 1999

;; (smallest-divisor 19999)
;; => 7

;; (smallest-divisor 199999)
;; => 199999



;;; x22

(define (search-for-primes initial final)
  (define (iter n)
    (timed-prime-test n)
    (if (< n (- final 1))
        (iter (+ n 2))))
  (if (even? initial)
      (iter (+ initial 1))
      (iter initial)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (get-internal-run-time))
  (newline))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (get-internal-run-time)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Square root order of growth seems close to the truth.

(define primes '(1009
                 1013
                 1019
                 10007
                 10009
                 10037
                 100003
                 100019
                 100043
                 1000003
                 1000033
                 1000037))



;;; x23

(define (timed-prime-test2 n)
  (display n)
  (start-prime-test2 n (get-internal-run-time))
  (newline))

(define (start-prime-test2 n start-time)
  (if (prime2? n)
      (report-prime (- (get-internal-run-time)
                       start-time))))

(define (prime2? n)
  (= (smallest-divisor2 n) n))

(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (define (next)
    (if (even? test-divisor)
        (+ 1 test-divisor)
        (+ 2 test-divisor)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next)))))

;; Sometimes ratio of periods of time for given primes is about 2. Sometimes.



;;; x24

(define (timed-fast-prime-test n)
  (display n)
  (start-fast-prime-test n (get-internal-run-time))
  (newline))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (get-internal-run-time)
                       start-time))))

;; Ratio of periods of time for 1000 and 1000000 isn't 2.



;;; x25

(define (timed-fast-prime-test2 n)
  (display n)
  (start-fast-prime-test2 n (get-internal-run-time))
  (newline))

(define (start-fast-prime-test2 n start-time)
  (if (fast-prime2? n 10)
      (report-prime (- (get-internal-run-time)
                       start-time))))

(define (fast-prime2? n times)
  (cond ((= times 0) #t)
        ((fermat-test2 n) (fast-prime2? n (- times 1)))
        (else #f)))

;; a < n => a mod n = a
(define (fermat-test2 n)
  (define (try-it a)
    (= a (expmod2 a n n)))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod2 base exp m)
  (remainder (my-fast-expt-iter base exp)
             m))

(define (my-fast-expt-iter base exponent)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter base exponent 1))

;; This implementation works more more slower. Ratio of periods of
;; time is about 2000 for "primes" list. Reason why is exponentiation:
;; big exponents, big powers and calculation of remainder for it.



;; Optional
;; More useful stuff for timing.
