(define (add-interval i j)
  (make-interval (+ (lower-bound i) (lower-bound j))
                 (+ (upper-bound i) (upper-bound j))))

(define (mul-interval i j)
  (let ((p1 (* (lower-bound i) (lower-bound j)))
        (p2 (* (lower-bound i) (upper-bound j)))
        (p3 (* (upper-bound i) (lower-bound j)))
        (p4 (* (upper-bound i) (upper-bound j))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval i j)
  (mul-interval i (make-interval (/ 1.0 (upper-bound j))
                                 (/ 1.0 (lower-bound j)))))


;;; x7

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))


;;; x8

(define (sub-interval i j)
  (make-interval (- (lower-bound i) (upper-bound j))
                 (- (upper-bound i) (lower-bound j))))


;;; x9

;; l - lower-bound (of interval)
;; u - upper-bound
;; c - center
;; w - width
;; addition:
;; l = (c1 - w1) + (c2 - w2) = (c1 + c2) - (w1 + w2)
;; u = (c1 + w1) + (c2 + w2) = (c1 + c2) + (w1 + w2)
;; c = c1 + c2
;; w = w1 + w2
;; subtraction:
;; Do not be confused with a formula for subtraction because sign is
;; matter and the formula works always.
;; l/u = l1 - u2
;; u/l = u1 - l2
;; c = c1 - c2
;; w = w1 + w2
;; multiplication (example):
;; l = l1 * l2 = (c1 - w1) * (c2 - w2) = c1 c2 - w1 c2 - w2 c1 + w1 w2
;; u = u1 * u2 = (c1 + w1) * (c2 + w2) = c1 c2 + w1 c2 + w2 c1 + w1 w2
;; c = c1 c2 + w1 w2
;; w = w1 c2 + w2 c1
;; division (example):
;; l = l1 * 1/u2 = (c1 - w1)/(c2 + w2)
;; u = u1 * 1/l2 = (c1 + w1)/(c2 - w2)
;; c = (l1 l2 + u1 u2)/(2 l2 u2) = (c1 c2 + w1 w2)/(c2^2 - w2^2)
;; w = u - c = (w1 c2 + w2 c1)/(c2^2 - w2^2)


;;; x10

(define (div2-interval i j)
  (if (contains-zero? j)
      (error "Divisor interval contains zero.")
      (mul-interval i (make-interval (/ 1.0 (upper-bound j))
                                     (/ 1.0 (lower-bound j))))))

(define (contains-zero? i)
  (let ((l (lower-bound i))
        (u (upper-bound i)))
    (and (>= u 0) (<= l 0))))


;;; x11

(define (mul2-interval i j)
  (let ((l lower-bound)
        (u upper-bound))
    (define (+? i) (or (zero? (l i)) (positive? (l i))))
    (define (-? i) (or (zero? (u i)) (negative? (u i))))
    (define (+/-? i) (not (or (+? i) (-? i))))
    (cond ((and (+? i) (+? j)) (make-interval (* (l i) (l j))
                                              (* (u i) (u j))))
          ((and (-? i) (-? j)) (make-interval (* (l i) (l j))
                                              (* (u i) (u j))))
          ((and (+? i) (-? j)) (make-interval (* (l i) (u j))
                                              (* (u i) (l j))))
          ((and (-? i) (+? j)) (make-interval (* (l i) (u j))
                                              (* (u i) (l j))))
          ((and (+? i) (+/-? j)) (make-interval (* (u i) (l j))
                                                (* (u i) (u j))))
          ((and (+/-? i) (+? j)) (make-interval (* (l i) (u j))
                                                (* (u i) (u j))))
          ((and (+/-? i) (-? j)) (make-interval (* (u i) (l j))
                                                (* (l i) (l j))))
          ((and (-? i) (+/-? j)) (make-interval (* (l i) (u j))
                                                (* (l i) (l j))))
          ((and (+/-? i) (+/-? j)) (make-interval (min (* (l i) (u j))
                                                       (* (u i) (l j)))
                                                  (max (* (l i) (l j))
                                                       (* (u i) (u j))))))))


;;; x12

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (* (/ (width i) (abs (center i))) 100.0))


;;; x13

;; If width of interval is much less than center of interval, it is
;; possible to use approximate formula. For example:
;; c - center of interval
;; w - width
;; l - lower-bound
;; u - upper-bound
;; p - percent
;; w1, w2 << c1, c2
;; l1 * l2 = (c1 - w1) * (c2 - w2) = c1 c2 - w1 c2 - w2 c1 + w1 w2 ~=
;; ~= c1 c2 - w1 c2 - w2 c1
;; c = c1 c2
;; w = w1 c2 + w2 c1
;; p = w/c = (w1/c1 + w2/c2) * 100 = p1 + p2
(define (mul3-interval i j)
  (let ((ci (center i))
        (cj (center j))
        (pi (percent i))
        (pj (percent j)))
    (if (and (< pi 10)
             (< pj 10))
        (if (or (and (> ci 0) (> cj 0))
                (and (< ci 0) (< cj 0)))
            (make-center-percent (* ci cj) (+ pi pj))
            (make-center-percent (* ci cj) (- pi pj)))
        (mul2-interval i j))))


;;; x14-15

;; For small percents result percent is about sum of percents, I
;; think. This is reason why par2 is better (more accurate) than par1:
;; smaller number of operations on percents.

(define (par1 r1 r2)
  (div2-interval (mul-interval r1 r2)
                 (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div2-interval one (add-interval (div2-interval one r1)
                                     (div2-interval one r2)))))

(define (make-intervals max-center max-percent n)
  (define (c) (* max-center (/ (+ 1 (random 100)) 100.0)))
  (define (p) (* max-percent (/ (+ 1 (random 100)) 100.0)))
  (make-list (lambda (n) (make-center-percent (c) (p)))
             n))

(define (make-list term n)
  (define (iter lst n)
    (if (< n 0)
        lst
        (iter (cons (term n) lst) (- n 1))))
  (iter '() n))

(define (print-intervals intervals)
  (print-list intervals identity))

(define (print-intervals-c-p intervals)
  (print-list intervals center-percent))

(define (center-percent i) (cons (center i) (percent i)))

(define (print-list lst selector)
  (cond ((eq? lst '()) (newline))
        (else (display (selector (car lst)))
              (newline)
              (print-list (cdr lst) selector))))

(define (print-table lst1 lst2 lst3 selector)
  (cond ((eq? lst1 '()) (newline))
        (else (display (selector (car lst1)))
              (display " ")
              (display (selector (car lst2)))
              (display " ")
              (display (selector (car lst3)))
              (display " ")
              (newline)
              (print-table (cdr lst1) (cdr lst2) (cdr lst3) selector))))

(define intervals (make-intervals 10 100 20))
(define narrower-intervals (make-intervals 10 10 20))
(define A-div-A (map (lambda (i) (div-interval i i))
                     narrower-intervals))
(define A-div-B (map (lambda (i) (div-interval i (make-interval 1 1.2)))
                     narrower-intervals))
(define par1-lst (map (lambda (i) (par1 i (make-interval 1 1.2)))
                      narrower-intervals))
(define par2-lst (map (lambda (i) (par2 i (make-interval 1 1.2)))
                      narrower-intervals))
