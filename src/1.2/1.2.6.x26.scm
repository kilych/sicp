(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (square x) (* x x))


(define (expmod-slow base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (* (expmod-slow base (/ exp 2) m)
                                   (expmod-slow base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod-slow base (- exp 1) m))
                         m))))

;; Tree recursion. Amount of steps is O(2^log(n)) = O(n).
