(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Substitution - applicative order:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;; Four (4) calls for remainder.

;; Substitution - normal order:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (= (remainder 206 40) 0)
;; (= 6 0)
;; (gcd (remainder 206 40) (remainder 40
;;                                    (remainder 206 40)))
;; (= (remainder 40
;;               (remainder 206 40))
;;    0)
;; (= (remainder 40
;;               6)
;;    0)
;; (= 4 0)
;; (gcd (remainder 40
;;                 (remainder 206 40))
;;      (remainder (remainder 206 40)
;;                 (remainder 40
;;                            (remainder 206 40))))
;; Briefly:
;; (gcd (remainder (remainder 206 40)
;;                 (remainder 40
;;                            (remainder 206 40)))
;;      (remainder (remainder 40
;;                            (remainder 206 40))
;;                 (remainder (remainder 206 40)
;;                            (remainder 40
;;                                       (remainder 206 40)))))
;; (remainder (remainder 206 40)
;;            (remainder 40
;;                       (remainder 206 40)))
;; 2
;; 18 calls for remainder.

;; How number of calls for normal order rises?
;; n iteration: a(n-1) a(n)
;; n+1 iteration: a(n) a(n-1) + a(n) + 1
;; where a(n) is number of deferred procedure calls (DPC) in second
;; argument of gcd in n iteration.
;; Number of calls rises like fib(k), where k is number of
;; iterations. fib(k) ~ phi^k, but k ~ log(smaller init number) according
;; to Lame's theorem, therefore, number of calls rises like smaller
;; init number, that is linear growth. Experimental proof needs lazy
;; interpreter.
