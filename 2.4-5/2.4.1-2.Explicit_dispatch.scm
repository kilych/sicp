(define (add z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;;; Generic constructors and selectors
;; untagging on this level

(define (make-from-real-imag x y) (make-from-real-imag-rect x y))
(define (make-from-mag-ang r a) (make-from-mag-ang-polar r a))

;; real-part, imag-part, magnitude, angle for complex numbers are
;; implemented in Scheme yet.
(define (real-part z)
  (cond ((rectangular? z) (real-part-rect (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unexpected type of data:" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rect (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unexpected type of data:" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rect (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unexpected type of data:" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rect (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unexpected type of data:" z))))

(define (rectangular? z) (eq? 'rect (type-tag z)))
(define (polar? z) (eq? 'polar (type-tag z)))


;; Now the selectors can not work with corresponding constructors,
;; because tagging is on this level but untagging is on upper level.


;;; Rectangular implementation

(define (make-from-real-imag-rect x y)
  (attach-tag 'rect (cons x y)))

(define (make-from-mag-ang-rect r a)
  (attach-tag 'rect (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-rect z) (car z))

(define (imag-part-rect z) (cdr z))

(define (magnitude-rect z)
  (let ((x (real-part-rect z))
        (y (imag-part-rect z)))
    (sqrt (+ (* x x) (* y y)))))

(define (angle-rect z)
  (atan (imag-part-rect z) (real-part-rect z)))


;;; Polar implementation

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (* x x) (* y y)))
                           (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))


;;; Type-tagging stuff

(define (attach-tag tag contents) (cons tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))


;;; Testing stuff

(define (print-complex-nums nums)
  (for-each (lambda (z) (display z) (newline))
            nums))

(define complex-nums (map (lambda (pair)
                            (make-from-real-imag (car pair)
                                                 (cadr pair)))
                          '((0 0)
                            (1 0)
                            (1 -1)
                            (-1 1)
                            (0 2)
                            (1 2)
                            (-1 -4)
                            (-3 3.5))))

(define z0 (car complex-nums))
(define z1 (cadr complex-nums))

(define complex-nums2 (map (lambda (z) (sub (add (div (mul z z) z) z) z))
                           (cdr complex-nums)))
(define complex-nums3 (map (lambda (z) (add (div z z1) z0))
                           complex-nums))
(define complex-nums4 (map (lambda (z) (div z z))
                           (cdr complex-nums)))


;;; Math stuff
;; not necessary: (atan y x) does it

(define pi 3.141592654)

(define (slope-angle x y)
  (cond ((= x 0) (if (> y 0) (/ pi 2) (* pi 3/2)))
        ((< x 0) (- pi (atan (/ y (- x)))))
        (else (atan (/ y x)))))
