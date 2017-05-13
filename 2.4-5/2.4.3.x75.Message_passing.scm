;;; Generic procedures
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

;;; Generic selectors and constructors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (* x x) (* y y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unexpected operator:" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unexpected operator:" op))))
  dispatch)

;;;
(define (apply-generic op arg) (arg op))

;;; Testing stuff
(define (print-complex-nums nums) (for-each print-complex-num nums))

(define (print-complex-num z)
  (display (list (real-part z) (imag-part z)))
  (newline))

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
