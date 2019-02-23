(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (cube x) (* x x x))


(define (pi-sum n)
  (* 8 (sum-pi-terms 1 n)))

(define (sum-pi-terms a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (sum-pi-terms (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-squares a b)
  (sum square a inc b))

(define (square x) (* x x))

(define (inc n) (+ n 1))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2)) add-dx b)))



;; x29

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2 h)))
  (* (/ h 3) (+ (f a)
                (* 4 (sum f (+ a h) add-2h (- b h)))
                (* 2 (sum f (+ a (* 2 h)) add-2h (- b (* 2 h))))
                (f b))))

;; For integers limits it is without round-off error, because call has
;; only integers.
;; For 0 and 1.0 limits it works worse than simple procedure integral above.



;; x30

(define (sum-iter term a next b)
  (define (iter a accum)
    (if (> a b)
        accum
        (iter (next a) (+ accum (term a)))))
  (iter a 0))



;; x31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (factorial n)
  (product iden 1 inc n))

(define (iden x) x)


(define (pi-product n)
  (define (pi-term x) (/ (* x (+ x 2)) (square (+ x 1))))
  (define (pi-next x) (+ x 2))
  (* 4 (product pi-term 2.0 pi-next n)))


(define (product-iter term a next b)
  (define (iter a accum)
    (if (> a b)
        accum
        (iter (next a) (* accum (term a)))))
  (iter a 1))
