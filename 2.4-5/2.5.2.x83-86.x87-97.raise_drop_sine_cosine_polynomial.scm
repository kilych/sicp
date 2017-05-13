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

;;;with-tag
;; In SICP way local package selectors are compatible with local
;; constructors because SICP adds tagging when put. Earlier I was
;; adding tagging when make, but I adopted the SICP way and determined
;; with-tag procedure that puts
;; (lambda (. args) (tag (apply proc args))) instead of proc.

;;; NOTE: If we have RAISE from A to B, PROJECT from B to A is not
;; necessary, but if we have PROJECT from B to A, RAISE from A to B should
;; be available, because our DROP works this way.

;;; Dependencies
(use-modules (srfi srfi-1))             ; need for fold-right
(load "./table.scm")

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

;;; Package installers
(define (install-scheme-number-package)
  (define (make x) x)
  ;; x97:
  (define (reduce-integers n d)
    (let ((g (gcd n d))) (list (/ n g) (/ d g))))
  (define (raise x) (make-complex-from-real-imag x 0))
  ;; (define (project x) (make-rational (floor x) 1))
  ;; Adding procs to proc-table
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  ;; (put 'exponent '(scheme-number scheme-number) expt)
  (put 'gcd '(scheme-number scheme-number) gcd)
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  (put '> '(scheme-number scheme-number) >)
  (put 'make 'scheme-number make)
  ;; unary
  (put 'add '(scheme-number) identity)
  (put 'sub '(scheme-number) identity)
  (put 'mul '(scheme-number) identity)
  (put 'div '(scheme-number) identity)
  (put 'change-of-sign '(scheme-number) -)
  (put 'square-root '(scheme-number) sqrt)
  (put 'equ? '(scheme-number) (lambda (x) #t))
  ;; trigonometric
  (put 'sine '(scheme-number) sin)
  (put 'cosine '(scheme-number) cos)
  (put 'arctangent '(scheme-number scheme-number) atan)
  ;; raise and project
  (put 'raise '(scheme-number) raise)
  ;; (put proc-table 'project '(scheme-number) project)
  'done)

(define (install-rational-package)
  ;; Selectors
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; Constructor
  ;; works too:
  ;; (define (make-rat n d)
  ;;   (if (=zero? d)
  ;;       (error "MAKE-RAT: Zero denominator" (list n d))
  ;;       (let ((g (greatest-common-divisor n d)))
  ;;         (cons (div n g) (div d g)))))
  ;; x97:
  (define (make-rat n d)
    (if (=zero? d)
        (error "MAKE-RAT: Zero denominator" (list n d))
        (let ((R (reduce n d))) (cons (car R) (cadr R)))))
  ;; Operations
  (define (add-rat x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make-rat (add (mul n q) (mul p d))
                (mul d q))))
  (define (sub-rat x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make-rat (sub (mul n q) (mul p d))
                (mul d q))))
  (define (mul-rat x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make-rat (mul n p)
                (mul d q))))
  (define (div-rat x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make-rat (mul n q)
                (mul d p))))
  (define (change-of-sign-rat x)
    (make-rat (change-of-sign (numer x)) (denom x)))
  ;; Predicates
  (define (equ-rat? x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (equ? (mul n q) (mul p d))))
  (define (zero-rat? x) (=zero? (numer x)))
  ;; raise
  (define (raise x) (make-scheme-number (div (numer x) (denom x))))
  ;; Adding procs to proc-table
  ;; In SICP tagging is when putting procs to table for all packages.
  (define (tag q) (attach-tag 'rational q))
  (define (with-tag proc) (lambda (. args) (tag (apply proc args))))
  (put 'add '(rational rational) (with-tag add-rat))
  (put 'sub '(rational rational) (with-tag sub-rat))
  (put 'mul '(rational rational) (with-tag mul-rat))
  (put 'div '(rational rational) (with-tag div-rat))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) zero-rat?)
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'make 'rational (with-tag make-rat))
  ;; unary
  (put 'add '(rational) with-tag)
  (put 'sub '(rational) with-tag)
  (put 'mul '(rational) with-tag)
  (put 'div '(rational) with-tag)
  (put 'change-of-sign '(rational) (with-tag change-of-sign-rat))
  (put 'equ? '(rational) (lambda (p) #t))
  ;; trigonometric
  (put 'arctangent '(rational rational)
       (lambda (y x)
         (let ((p (div-rat y x))) (atan (numer p) (denom p)))))
  ;; raise
  (put 'raise '(rational) raise)
  'done)

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

;;; x87-
;; x87: =zero?
;; x88: subtraction of polynomials
;; x89-90: implementation of dense polynomials and generalization of
;; termlist operations
;; x91: division of polynomials
;; x92: variable coercion
;; x93: generic rational
;; x94: generic greatest-common-divisor
;; x95: issue with rational coeffs in gcd-polys - explain
;; x96: gcd-polys with pseudoremainder-poly
;; 97: generic reduce: reduce-integers and reduce-polys
(define (install-sparse-package)
  ;; Term operations
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (make-term order coeff) (list order coeff))
  (define (zero-term? term) (=zero? (coeff term)))
  (define (change-of-sign-term term)
    (make-term (order term) (change-of-sign (coeff term))))
  ;; it's magic here: if we use generic add, then coeffs can have any
  ;; of known types, for example, complex, polynomial, etc.
  (define (add-term-to-term t1 t2)
    (make-term (order t1) (add (coeff t1) (coeff t2))))
  (define (mul-term-by-term t1 t2)
    (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2))))
  ;; Termlist operations
  (define (make-zero-termlist) '())
  (define (make-from-order-coeff ord c) (list (make-term ord c)))
  (define (leading-term termlist) (car termlist))
  (define (lower-terms termlist) (cdr termlist))
  ;; (define (lower-terms terms)
  ;;   (if (zero-termlist? terms)
  ;;       (make-zero-termlist)
  ;;       (cdr terms)))
  (define (normalize L) (filter (lambda (t) (not (zero-term? t))) L))
  ;; predicates
  (define (zero-termlist? termlist) (null? (normalize termlist)))
  (define (equ-termlists? L1 L2)
    (define (iter L1 L2)
      (cond ((and (null? L1) (null? L2)) #t)
            ((or (null? L1) (null? L2)) #f)
            ((and (= (order (leading-term L1))
                     (order (leading-term L2)))
                  (equ? (coeff (leading-term L1))
                        (coeff (leading-term L2))))
             (iter (lower-terms L1) (lower-terms L2)))
            (else #f)))
    (iter (normalize L1) (normalize L2)))
  (define (termlist-order terms)
    (if (zero-termlist? terms) 0 (order (leading-term terms))))
  (define (leading-coeff terms)
    (if (zero-termlist? terms) 0 (coeff (leading-term terms))))
  (define (add-term term termlist)      ;"adjoint-term"
    (if (=zero? (coeff term))
        termlist
        ;; we assume, that term has higher order than terms in term-list
        (cons term termlist)))
  ;; Arithmetic operations
  (define (add-terms L1 L2)
    (cond ((zero-termlist? L1) L2)
          ((zero-termlist? L2) L1)
          (else (let ((t1 (leading-term L1))
                      (t2 (leading-term L2))
                      (rest1 (lower-terms L1))
                      (rest2 (lower-terms L2)))
                  (cond ((> (order t1) (order t2))
                         (add-term t1 (add-terms rest1 L2)))
                        ((< (order t1) (order t2))
                         (add-term t2 (add-terms L1 rest2)))
                        ((= (order t1) (order t2))
                         (add-term (add-term-to-term t1 t2)
                                   (add-terms rest1 rest2))))))))
  (define (change-of-sign-terms termlist)
    (if (zero-termlist? termlist)
        (make-zero-termlist)
        (map change-of-sign-term termlist)))
  (define (sub-terms L1 L2) (add-terms L1 (change-of-sign-terms L2)))
  (define (mul-term term termlist)
    (if (zero-termlist? termlist)
        (make-zero-termlist)
        (map (lambda (t) (mul-term-by-term t term)) termlist)))
  (define (mul-terms L1 L2)
    (if (zero-termlist? L2)
        (make-zero-termlist)
        (fold-right add-terms
                    (make-zero-termlist)
                    (map (lambda (term) (mul-term term L1)) L2))))
  ;; Adding procs to proc-table
  (define (tag proc)
    (lambda (. args) (attach-tag 'sparse (apply proc args))))
  ;; arithmetic
  (put 'add '(sparse sparse) (tag add-terms))
  (put 'sub '(sparse sparse) (tag sub-terms))
  (put 'mul '(sparse sparse) (tag mul-terms))
  (put 'change-of-sign '(sparse) (tag change-of-sign-terms))
  ;; predicates
  (put '=zero? '(sparse) zero-termlist?)
  (put 'equ? '(sparse sparse) equ-termlists?)
  ;; selectors
  (put 'order '(sparse) termlist-order)
  (put 'leading-coeff '(sparse) leading-coeff)
  (put 'lower-terms '(sparse) (tag lower-terms))
  ;; constructors
  (put 'make-from-order-coeff 'sparse
       (tag make-from-order-coeff))
  ;; project
  ;; (put 'project '(sparse) leading-coeff)
  'done)

;; Order from lowest to highest degree ("ascending")
(define (install-dense-asc-package)
  ;; Constructors
  (define (make-zero-termlist) '())
  (define (make-termlist-from-list L) L)
  ;; Selectors
  (define (lowest-term termlist) (car termlist))
  (define (higher-terms termlist) (cdr termlist))
  ;; Predicates
  (define (zero-termlist? termlist)
    (cond ((null? termlist) #t)
          ((=zero? (lowest-term termlist))
           (zero-termlist? (higher-terms termlist)))
          (else #f)))
  (define (equ-termlists? L1 L2)
    (cond ((and (null? L1) (null? L2)) #t)
          ((or (null? L1) (null? L2)) #f)
          ((equ? (lowest-term L1) (lowest-term L2))
           (equ-termlists? (higher-terms L1) (higher-terms L2)))
          (else #f)))
  ;; Arithmetic
  (define (add-terms L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else (let ((t1 (lowest-term L1))
                      (t2 (lowest-term L2))
                      (r1 (higher-terms L1))
                      (r2 (higher-terms L2)))
                  (cons (add t1 t2) (add-terms r1 r2))))))
  (define (change-of-sign-terms termlist)
    (if (zero-termlist? termlist)
        (make-zero-termlist)
        (map change-of-sign termlist)))
  (define (sub-terms L1 L2) (add-terms L1 (change-of-sign-terms L2)))
  (define (mul-terms L1 L2)
    (define (raise-order termlist) (cons 0 termlist))
    (define (repeat-raise count termlist)
      (if (= count 0)
          termlist
          (repeat-raise (1- count) (raise-order termlist))))
    (define (prods L1 L2 n acc)
      (if (null? L1)
          acc
          (let ((te (lowest-term L1))
                (rest (higher-terms L1)))
            (prods rest
                   L2
                   (1+ n)
                   (cons (repeat-raise n (map (lambda (t) (mul t te))
                                              L2))
                         acc)))))
    ;; fold doesn't work in this case (???):
    (fold-right add-terms (make-zero-termlist) (prods L1 L2 0 '())))
  ;; raise
  (define (raise-to-sparse L)
    (define (iter order L acc)
      (if (null? L)
          (attach-tag 'sparse acc)
          (let ((coeff (lowest-term L)))
            (let ((add-term (lambda (L)
                              (if (=zero? coeff)
                                  L
                                  (cons (list order coeff) L)))))
              (iter (1+ order) (higher-terms L) (add-term acc))))))
    (iter 0 L '()))
  ;; Adding procs to proc-table
  (define (tag L) (attach-tag 'dense-asc L))
  (define (with-tag proc) (lambda (. args) (tag (apply proc args))))
  ;; constructors
  (put 'make-from-list 'dense-asc (with-tag make-termlist-from-list))
  ;; arithmetic
  (put 'add '(dense-asc dense-asc) (with-tag add-terms))
  (put 'sub '(dense-asc dense-asc) (with-tag sub-terms))
  (put 'mul '(dense-asc dense-asc) (with-tag mul-terms))
  (put 'change-of-sign '(dense-asc) (with-tag change-of-sign-terms))
  ;; predicates
  (put '=zero? '(dense-asc) zero-termlist?)
  (put 'equ? '(dense-asc dense-asc) equ-termlists?)
  ;; raise and project
  (put 'raise '(dense-asc) raise-to-sparse)
  (put 'project '(dense-asc)
       (lambda (L) ((get 'make-from-list 'dense-desc) (reverse L))))
  'done)

;; Descending order
(define (install-dense-desc-package)
  ;; Constructors
  (define (make-zero-termlist) '())
  (define (make-zero-order-termlist coeff) (list coeff))
  (define (make-termlist-from-list L) L)
  ;; Predicates
  (define (zero-termlist? termlist)
    (cond ((null? termlist) #t)
          ((=zero? (car termlist))
           (zero-termlist? (cdr termlist)))
          (else #f)))
  ;; Selectors
  (define (order terms)
    (if (zero-termlist? terms) 0 (1- (length terms))))
  (define (leading-coeff terms)
    (if (zero-termlist? terms) 0 (car terms)))
  (define (lower-terms terms)
    (if (zero-termlist? terms) (make-zero-termlist) (cdr terms)))
  ;; Adding procs to proc-table
  (define (tag L) (attach-tag 'dense-desc L))
  (define (with-tag proc) (lambda (. args) (tag (apply proc args))))
  ;; constructors
  (put 'make-zero-order 'dense-desc
       (with-tag make-zero-order-termlist))
  (put 'make-from-list 'dense-desc
       (with-tag make-termlist-from-list))
  ;; predicates
  (put '=zero? '(dense-desc) zero-termlist?)
  ;; selectors
  (put 'order '(dense-desc) order)
  (put 'leading-coeff '(dense-desc) leading-coeff)
  (put 'lower-terms '(dense-desc) (with-tag lower-terms))
  ;; raise and project
  (put 'raise '(dense-desc)
       (lambda (L)
         ((get 'make-from-list 'dense-asc) (reverse L))))
  ;; (put 'project '(dense-desc) leading-coeff)
  'done)

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'poly p))
  ;; Constructors
  (define (make-poly var termlist) (cons var termlist))
  (define (make-zero-poly var) (make-poly var '()))
  (define (make-zero-order-poly x)
    (make-poly 'free-var ((get 'make-zero-order 'dense-desc) x)))
  ;; Selectors
  (define (var poly) (car poly))
  (define (term-list poly) (cdr poly))
  (define (order-poly p) (order (term-list p)))
  (define (leading-coeff-poly p) (leading-coeff (term-list p)))
  (define (lower-terms-poly p)
    (make-poly (var p) (lower-terms (term-list p))))
  ;; Predicates
  (define (var? x) (symbol? x))
  (define (same-var? x y) (eq? x y))
  (define (var>? v1 v2)
    (let ((s1 (symbol->string v1))
          (s2 (symbol->string v2))) (string>? s1 s2)))
  (define (var<? v1 v2)
    (let ((s1 (symbol->string v1))
          (s2 (symbol->string v2))) (string<? s1 s2)))
  (define (free-var? v) (eq? 'free-var v))
  (define (zero-poly? p) (=zero? (term-list p)))
	(define (equ-poly? p1 p2)
    (let ((v1 (var p1))
          (v2 (var p2)))
      (and (or (same-var? v1 v2) (free-var? v1) (free-var? v2))
           (equ? (term-list p1) (term-list p2)))))
  ;; Arithmetic
  (define (add-poly p1 p2)
    (make-poly (var p1) (add (term-list p1) (term-list p2))))
  (define (change-of-sign-poly p)
    (make-poly (var p) (change-of-sign (term-list p))))
  ;; If we with-tag in make-poly, p1 is untagged and
  ;; (change-of-sign-poly p2) is tagged - the stuff are incompatible.
  (define (sub-poly p1 p2) (add-poly p1 (change-of-sign-poly p2)))
  (define (mul-poly p1 p2)
    (make-poly (var p1) (mul (term-list p1) (term-list p2))))
  (define (div-poly p1 p2)
    (define (make-L ord c)
      ((get 'make-from-order-coeff 'sparse) ord c))
    (define (div-terms L1 L2 quotient-L)
      (let ((ord1 (order L1))
            (ord2 (order L2))
            (c1 (leading-coeff L1))
            (c2 (leading-coeff L2)))
        (if (or (=zero? L1) (< ord1 ord2))
            (list quotient-L L1)
            (let ((factor (make-L (- ord1 ord2) (div c1 c2)))
                  (rest1 (lower-terms L1))
                  (rest2 (lower-terms L2)))
              (div-terms (sub rest1 (mul factor rest2))
                         L2
                         (add factor quotient-L))))))
    (if (zero-poly? p2)
        (error "DIV-POLY: Zero divisor" p2)
        (map (lambda (L) (make-poly (var p1) L))
             (div-terms (term-list p1) (term-list p2) (list 'sparse)))))
  (define (quotient-poly p1 p2) (car (div-poly p1 p2)))
  (define (remainder-poly p1 p2) (cadr (div-poly p1 p2)))
  (define (pseudodiv-poly p1 p2)
    (let ((ord1 (order-poly p1))
          (ord2 (order-poly p2))
          (c (leading-coeff-poly p2)))
      (let ((integerizing-factor (exponent c (+ 1 ord1 (- ord2)))))
        (div-poly (contents (mul (tag p1) integerizing-factor)) p2))))
  (define (pseudoremainder-poly p1 p2) (cadr (pseudodiv-poly p1 p2)))
  (define (gcd-polys p1 p2)
    (define (gcd-coeffs g p)
      (if (zero-poly? p)
          g
          (gcd-coeffs (greatest-common-divisor (leading-coeff-poly p) g)
                      (lower-terms-poly p))))
    (if (zero-poly? p2)
        (let ((gp (make-zero-order-poly (gcd-coeffs 0 p1))))
          (if (zero-poly? p1) p1 (quotient-poly p1 gp)))
        (gcd-polys p2 (pseudoremainder-poly p1 p2))))
  ;; x97:
  (define (reduce-polys p1 p2)
    (let ((g (gcd-polys p1 p2)))
      (map tag (list (quotient-poly p1 g) (quotient-poly p2 g)))))
  (define (div-poly-or-make-rational p1 p2)
    (let ((quo-and-rem (div-poly p1 p2)))
      (let ((quo (car quo-and-rem))
            (rem (cadr quo-and-rem)))
        (if (zero-poly? rem)
            (tag quo)
            (make-rational (tag p1) (tag p2))))))
  ;; variable coercion
  (define (coerce-vars p1 p2)
    (let ((v1 (var p1))
          (v2 (var p2))
          (ord1 (order-poly p1))
          (ord2 (order-poly p2)))
      (cond ((not (and (var? v1) (var? v2))) ;should check here???
             (error "COERCE-VARS: Not variable" (list p1 p2)))
            ((same-var? v1 v2) (list p1 p2))
            ((and (= 0 ord1) (poly? (leading-coeff-poly p1)))
             (coerce-vars (contents (leading-coeff-poly p1)) p2))
            ((and (= 0 ord2) (poly? (leading-coeff-poly p2)))
             (coerce-vars p1 (contents (leading-coeff-poly p2))))
            ((or (= 0 ord1) (free-var? v1))
             (coerce-vars (make-poly v2 (term-list p1)) p2))
            ((or (= 0 ord2) (free-var? v2))
             (coerce-vars p1 (make-poly v1 (term-list p2))))
            ((var>? v1 v2)
             (list p1 (make-poly v1 ((get 'make-zero-order 'dense-desc)
                                     (tag p2)))))
            ((var<? v1 v2)
             (list (make-poly v2 ((get 'make-zero-order 'dense-desc)
                                  (tag p1)))
                   p2))
            (else (error "COERCE-VARS:" (list p1 p2))))))
  ;; raise and project
  (define (raise p)
    (make-rational (tag p) (tag (make-zero-order-poly 1))))
  (define (project p) (leading-coeff-poly p))
  ;; Adding procs to proc-table
  (define (with-tag proc) (lambda (. args) (tag (apply proc args))))
  ;; variable coercion of arguments
  (define (coerce-args proc)
    (lambda (x y) (apply proc (coerce-vars x y))))
  ;; arithmetic
  (put 'add '(poly poly) (with-tag (coerce-args add-poly)))
  (put 'sub '(poly poly) (with-tag (coerce-args sub-poly)))
  (put 'mul '(poly poly) (with-tag (coerce-args mul-poly)))
  (put 'div '(poly poly) (coerce-args div-poly-or-make-rational))
  (put 'gcd '(poly poly) (with-tag (coerce-args gcd-polys)))
  (put 'reduce '(poly poly) (coerce-args reduce-polys))
  (put 'change-of-sign '(poly) (with-tag change-of-sign-poly))
  ;; constructors
  (put 'make-zero-order 'poly (with-tag make-zero-order-poly))
	(put 'make-from-var-list 'poly (with-tag make-poly))
  ;; predicates
  (put '=zero? '(poly) zero-poly?)
	(put 'equ? '(poly poly) equ-poly?)
  ;; selectors
  (put 'order '(poly) order-poly)
  (put 'leading-coeff '(poly) leading-coeff-poly)
  (put 'lower-terms '(poly) (with-tag lower-terms-poly))
  ;; raise and project
  ;; (put 'raise '(poly) raise) ;bad recursion (procedure number?) (???)
  (put 'project '(poly) project)
  'done)

;;; x84. Dispatch
(define (apply-generic op args)
  (define (proc op args) (get op (map type-tag args)))
	(define (result args)
    (let ((res (apply (proc op args) (map contents args))))
      (if (or (eq? op 'raise) (eq? op 'project) (boolean? res))
					res
					(drop res)))) ;until we call constructors without apply-generic
  (let ((len (length args)))
    (cond ((= len 0) (error "APPLY-GENERIC: No args") op)
          ((= len 1)
					 (let ((r (raise-until (lambda (x) (proc op (list x)))
																 (car args))))
						 (if r
								 (result (list r))
								 (let ((ra (raise-until (lambda (x) (proc op (list x)))
																				(drop (car args)))))
									 (if ra
											 (result (list ra))
											 (else
												(error "APPLY-GENERIC: No method for this type"
															 (cons op args))))))))
          ((= len 2)
					 (let ((a1 (car args))
								 (a2 (cadr args)))
						 (cond ((proc op args) (result args))
									 ((and (same-type? a1 a2) (proc 'raise (list a1)))
										(apply-generic op (map raise args)))
									 ((not (same-type? a1 a2))
										(let ((r1 (raise-until (lambda (x) (same-type? x a2))
																					 a1))
													(r2 (raise-until (lambda (x) (same-type? a1 x))
																					 a2)))
											(cond (r1 (apply-generic op (list r1 a2)))
														(r2 (apply-generic op (list a1 r2)))
														(else
														 (error "APPLY-GENERIC: Fail to raise args"
																		(cons op args))))))
									 (else (error "APPLY-GENERIC: No method for this types"
																(cons op args))))))
          (else (apply-generic op
                               (cons (apply-generic op
                                                    (list (car args)
                                                          (cadr args)))
                                     (cddr args)))))))

;;; Installing packages to(?) the table of procedures ("V-table")
(define proc-table (make-table))
(define (put op type-tags proc) (put-to proc-table op type-tags proc))
(define (get op type-tags) (get-from proc-table op type-tags))
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-sparse-package)
(install-dense-asc-package)
(install-dense-desc-package)
(install-polynomial-package)

;;; Testing stuff
(define (print-nums nums)
  (for-each (lambda (n) (display n) (newline)) nums))

(define scheme-nums (map (lambda (n) (make-scheme-number n))
                         '(0 1 -1 2 -4 -3 3.5 5)))
(define n0 (car scheme-nums))
(define n1 (cadr scheme-nums))
(define scheme-nums2 (map (lambda (n) (sub (add (div (mul n n) n) n) n))
                          (cdr scheme-nums)))
(define scheme-nums3 (map (lambda (n) (add (div n n1) n0)) scheme-nums))
(define scheme-nums4 (map (lambda (n) (div n n)) (cdr scheme-nums)))

(define rat-nums
  (map (lambda (pair) (make-rational (car pair) (cadr pair)))
       '((0 1) (1 1) (1 -1) (-1 1) (1 2) (-1 -4) (-3 5) (2 3))))
(define p0 (car rat-nums))
(define p1 (cadr rat-nums))
(define rat-nums2 (map (lambda (p) (sub (add (div (mul p p) p) p) p))
                       (cdr rat-nums)))
(define rat-nums3 (map (lambda (p) (add (div p p1) p0)) (cdr rat-nums)))
(define rat-nums4 (map (lambda (p) (div p p)) (cdr rat-nums)))

(define complex-nums
  (map (lambda (pair)
         (make-complex-from-real-imag (car pair) (cadr pair)))
       '((0 0) (1 0) (1 -1) (-1 1) (0 2) (1 2) (-1 -4) (-3 3.5))))
(define z0 (car complex-nums))
(define z1 (cadr complex-nums))
(define z2 (caddr complex-nums))
(define complex-nums2 (map (lambda (z) (sub (add (div (mul z z) z) z) z))
                           (cdr complex-nums)))
(define complex-nums3 (map (lambda (z) (add (div z z1) z0))
                           complex-nums))
(define complex-nums4 (map (lambda (z) (div z z)) (cdr complex-nums)))

(define po1 (make-poly-from-var-list 'x '(sparse (2 2) (0 2))))
(define po2 (make-poly-from-var-list 'x '(sparse (3 2) (0 2))))
(define rf (make-rational po2 po1))
(define po3
  (make-poly-from-var-list 'x '(sparse (4 1) (3 -1) (2 -2) (1 2))))
(define po4 (make-poly-from-var-list 'x '(sparse (3 1) (1 -1))))
(define poy (make-poly-from-var-list 'y '(sparse (3 2) (0 2))))

;;; x95
(define P1 (make-poly-from-var-list 'x '(dense-desc 1 -2 1)))
(define P2 (make-poly-from-var-list 'x '(dense-desc 11 0 1)))
(define P3 (make-poly-from-var-list 'x '(dense-desc 13 5)))
(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(define P1~ (greatest-common-divisor Q1 Q2))
;; P1~
;; => '(poly x sparse (2 444/169) (1 -888/169) (0 444/169))
;; Why are P1 and P1~ not equal?
