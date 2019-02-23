(define (add-rat x y)
  (let ((n (numer x))
        (d (denom x))
        (p (numer y))
        (q (denom y)))
    (make-rat (+ (* n q) (* p d))
              (* d q))))

(define (sub-rat x y)
   (let ((n (numer x))
         (d (denom x))
         (p (numer y))
         (q (denom y)))
     (make-rat (- (* n q) (* p d))
               (* d q))))

(define (mul-rat x y)
  (let ((n (numer x))
        (d (denom x))
        (p (numer y))
        (q (denom y)))
    (make-rat (* n p)
              (* d q))))

(define (div-rat x y)
  (let ((n (numer x))
        (d (denom x))
        (p (numer y))
        (q (denom y)))
    (make-rat (* n q)
              (* d p))))

(define (eq-rat? x y)
  (let ((n (numer x))
        (d (denom x))
        (p (numer y))
        (q (denom y)))
    (= (* n q) (* p d))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))



;;;;;;;;;

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))


;; (define (make-rat n d)
;;   (cons n d))

;; (define (numer x)
;;   (let ((g (gcd (car x) (cdr x))))
;;     (/ (car x) g)))

;; (define (denom x)
;;   (let ((g (gcd (car x) (cdr x))))
;;     (/ (cdr x) g)))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))



;;;;;;;;;

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))
