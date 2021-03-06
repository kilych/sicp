(define (install-rectangular-package)
  (define (tag x) (attach-tag 'rect x))
  ;; Selectors
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (let ((x (real-part z))
          (y (imag-part z)))
      (square-root (add (mul x x) (mul y y)))))
  (define (angle z) (arctangent (imag-part z) (real-part z)))
  ;; Constructors
  (define (make-from-real-imag x y) (tag (cons x y)))
  (define (make-from-mag-ang r a)
    (tag (cons (mul r (cosine a)) (mul r (sine a)))))
  ;; Arithmetics
  (define (change-of-sign-rect z)
    (make-from-real-imag (change-of-sign (real-part z))
                         (change-of-sign (imag-part z))))
  ;; Predicates
  (define (equ?-rect z1 z2) (and (equ? (real-part z1) (real-part z2))
                                 (equ? (imag-part z1) (imag-part z2))))
  (define (=zero?-rect z) (and (=zero? (real-part z))
                               (=zero? (imag-part z))))
  ;; Adding procs to proc-table
  (put 'equ? '(rect rect) equ?-rect)    ;x79
  (put '=zero? '(rect) =zero?-rect)
  (put 'real-part '(rect) real-part)
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) magnitude)
  (put 'angle '(rect) angle)
  (put 'change-of-sign '(rect) change-of-sign-rect)
  (put 'make-from-real-imag 'rect make-from-real-imag)
  (put 'make-from-mag-ang 'rect make-from-mag-ang)
  'done)

(define (install-polar-package)
  (define (tag x) (attach-tag 'polar x))
  ;; Selectors
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  ;; Constructors
  (define (make-from-real-imag x y)
    (tag (cons (square-root (add (mul x x) (mul y y)))
               (arctangent y x))))
  (define (make-from-mag-ang r a)
    (tag (cons r a)))
  ;; Predicates
  (define (equ?-polar z1 z2) (and (equ? (magnitude z1) (magnitude z2))
                                  (equ? (angle z1) (angle z2))))
  (define (=zero?-polar z) (=zero? (magnitude z)))
  ;; raise
  (define (raise z)
    ((get 'make-from-real-imag 'rect)
     (real-part z)
     (imag-part z)))
  ;; Adding procs to proc-table
  (put 'equ? '(polar polar) equ?-polar) ;x79
  (put '=zero? '(polar) =zero?-polar)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar make-from-real-imag)
  (put 'make-from-mag-ang 'polar make-from-mag-ang)
  ;; raise
  (put 'raise '(polar) raise)
  'done)

(define (install-complex-package)
  (define (tag z) (attach-tag 'complex z))
  (define (make-from-real-imag x y)
    (tag ((get 'make-from-real-imag 'rect) x y)))
  (define (make-from-mag-ang r a)
    (tag ((get 'make-from-mag-ang 'polar) r a)))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  ;; round-off errors (rounding errors) in pseudoremainder:
  ;; (define (mul-complex z1 z2)
  ;;   (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
  ;;                      (add (angle z1) (angle z2))))
  (define (mul-complex z1 z2)
    (make-from-real-imag (sub (mul (real-part z1) (real-part z2))
                              (mul (imag-part z1) (imag-part z2)))
                         (add (mul (real-part z1) (imag-part z2))
                              (mul (imag-part z1) (real-part z2)))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; project
  (define (project z) (make-scheme-number (real-part z)))
  ;; Adding procs to proc-table
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)
  (put 'div '(complex complex) div-complex)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex make-from-real-imag)
  (put 'make-from-mag-ang 'complex make-from-mag-ang)
  ;; unary
  (put 'add '(complex) tag)
  (put 'sub '(complex) tag)
  (put 'mul '(complex) tag)
  (put 'div '(complex) tag)
  (put 'change-of-sign '(complex)
       (lambda (z) (tag (change-of-sign z))))
  (put 'equ? '(complex) (lambda (z) #t))
  ;; raise and project
  (put 'raise '(complex)
       (lambda (z) (make-zero-order-poly (tag z))))
  (put 'project '(complex) project)
  'done)
