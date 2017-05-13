(define (filtered-accumulate combiner null-value predicate term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))


(define (sum-square-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next)
    (if (even? test-divisor)
        (+ 1 test-divisor)
        (+ 2 test-divisor)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (inc x) (+ x 1))


(define (product-coprimes n)
  (define (coprime-n? i)
    (= 1 (gcd n i)))
  (filtered-accumulate * 1 coprime-n? identity 1 inc (- n 1)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
