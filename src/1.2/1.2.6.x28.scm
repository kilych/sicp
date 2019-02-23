(define (miller-rabin-prime10? n)
  (miller-rabin-prime? n 10))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else #f)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= 1 (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-expmod base exp m)
  (define (squaremod-check n)
    (define squaremod (remainder (square n) m))
    (if (and (not (= n 1))
             (not (= n (- m 1)))
             (= 1 squaremod))
        0
        squaremod))
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-check (miller-rabin-expmod base
                                                                (/ exp 2)
                                                                m)))
        (else (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                         m))))

(define (square x) (* x x))

;; I cheated a little bit. I saw other's solution on rosettacode.org and
;; found two hints. I stuck at if condition in squaremod-check. m
;; or exp? Finally, m. Also, I wrote "(check (square (expmod ..)))"
;; instead "(squaremod-check (expmod ..))" where i define squaremod.
