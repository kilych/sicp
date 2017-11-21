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

(use-modules (srfi srfi-1))             ; need for fold-right

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
