;;; OPTIONAL:
;; Constructors for rational and scheme numbers can make this numbers
;; from rational arguments.
;; Constructors for complex numbers can make this numbers from complex
;; arguments. Now it's possible but not beautiful:
;; (add z1 (make-complex-from-real-imag z1 z2))
;; => (complex rect 2 complex rect 1 . -1)
;; Also:
;; do I need to raise args of binary operation, that have same type,
;; but haven't operation for this type?

;;; NOTE: Local package procedures, that have the same functionality,
;; need to have different name, for example, add-rat and add-complex
;; instead of add and add in both packages, because we need to use
;; generic add, sub, mul, etc. without naming clashes.

;;; with-tag
;; In SICP way local package selectors are compatible with local
;; constructors because SICP adds tagging when put. Earlier I was
;; adding tagging when make, but I adopted the SICP way and determined
;; with-tag procedure that puts
;; (lambda (. args) (tag (apply proc args))) instead of proc.

;;; NOTE: If we have RAISE from A to B, PROJECT from B to A is not
;; necessary, but if we have PROJECT from B to A, RAISE from A to B should
;; be available, because our DROP works this way.

;;; x83, 85
(define (raise x) (apply-generic 'raise (list x)))
(define (raise-until pred x)
  (cond ((pred x) x)
        ((or (not (get 'raise (list (type-tag x))))
             (same-type? x (raise x))) #f)
        (else (raise-until pred (raise x)))))
(define (project x) (apply-generic 'project (list x)))
(define (drop x)
	(if (and (expected-type? x)
           (get 'project (list (type-tag x)))
           (raise-until (lambda (y) (same-type? x y)) (project x))
           (equ? x (raise-until (lambda (y) (same-type? x y))
                                (project x))))
      (drop (project x))
      x))

;;; Generic arithmetic operations
(define (add x . y) (apply-generic 'add (cons x y)))
(define (sub x . y) (apply-generic 'sub (cons x y)))
(define (mul x . y) (apply-generic 'mul (cons x y)))
(define (div x . y) (apply-generic 'div (cons x y)))
;; (define (exponent x  y) (apply-generic 'exponent (list x y)))
(define (change-of-sign x) (apply-generic 'change-of-sign (list x)))
(define (square-root x) (apply-generic 'square-root (list x)))
(define (greatest-common-divisor x y) (apply-generic 'gcd (list x y)))

;; x97
(define (reduce x y) (apply-generic 'reduce (list x y)))

(define (exponent b n)
  (define (square x) (mul x x))
  (define (iter b n acc)
    (cond ((zero? n) acc)
          ((even? n) (iter (square b) (/ n 2) acc))
          (else (iter b (1- n) (mul b acc)))))
  (iter b n 1))

;;; x86. Generic trigonometric operations
(define (sine x) (apply-generic 'sine (list x)))
(define (cosine x) (apply-generic 'cosine (list x)))
(define (arctangent y x) (apply-generic 'arctangent (list y x)))

;;; Constructors
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-zero-order-poly x)
  ((get 'make-zero-order 'poly) x))
(define (make-poly-from-var-list v L)
	((get 'make-from-var-list 'poly) v L))

;;; Selectors
;; for rational nums
(define (numer p) (apply-generic 'numer (list p)))
(define (denom p) (apply-generic 'denom (list p)))
;; for complex nums
(define (real-part z) (apply-generic 'real-part (list z)))
(define (imag-part z) (apply-generic 'imag-part (list z)))
(define (magnitude z) (apply-generic 'magnitude (list z)))
(define (angle z) (apply-generic 'angle (list z)))
;; for polynomial
(define (order p) (apply-generic 'order (list p)))
(define (leading-coeff p) (apply-generic 'leading-coeff (list p)))
;; drop the highest degree term:
(define (lower-terms p) (apply-generic 'lower-terms (list p)))

;;; Predicates
;; OPTIONAL: arbitrary arity for equ?
(define (equ? x y) (apply-generic 'equ? (list x y)))
(define (=zero? x) (apply-generic '=zero? (list x)))
(define (greater-than? x y) (apply-generic '> (list x y)))
;; Type predicates
(define (poly? datum) (this-type? 'poly datum))

;;; Tagging stuff
(define (attach-tag tag contents)
  (if (eq? tag 'scheme-number)
      contents
      (cons tag contents)))
(define (type-tag datum)
	(cond ((number? datum) 'scheme-number)
        ((and (pair? datum) (symbol? (car datum))) (car datum))
        (else (error "TYPE-TAG: Unexpected type of data" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((and (pair? datum) (symbol? (car datum))) (cdr datum))
        (else (error "CONTENTS: Unexpected type of data"))))
(define (expected-type? datum)
  (or (number? datum) (and (pair? datum) (symbol? (car datum)))))
;; (define (same-type? x y) (eq? (type-tag x) (type-tag y)))
(define (same-type? x . y)
  (define (iter type-tags)
    (cond ((null? (cdr type-tags)) #t)
          ((let ((t1 (car type-tags))
                 (t2 (cadr type-tags))) (eq? t1 t2))
           (iter (cdr type-tags)))
          (else #f)))
  (iter (map type-tag (cons x y))))
(define (this-type? type datum) (eq? type (type-tag datum)))
