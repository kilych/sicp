;;; x81
;; a. Bad recursion: (apply-generic ’exp z1 z2) calls itself
;; endlessly.
;; b. All stuff works right. For the same type coercion is not really
;; necessary. If the proc exists we just apply it on args of the
;; same type. If the proc doesn't we just go down to error.

;; c. Definition from SICP:
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (= (length args) 2)
;;               (let ((type1 (car type-tags))
;;                     (type2 (cadr type-tags))
;;                     (a1 (car args))
;;                     (a2 (cadr args)))
;;                 ;; x81.c
;;                 (if (eq? type1 type2)
;;                     (error "Нет метода для этих типов"
;;                            (list op type-tags))
;;                     (let ((t1->t2 (get-coercion type1 type2))
;;                           (t2->t1 (get-coercion type2 type1)))
;;                       (cond (t1->t2
;;                              (apply-generic op (t1->t2 a1) a2))
;;                             (t2->t1
;;                              (apply-generic op a1 (t2->t1 a2)))
;;                             (else
;;                              (error "Нет метода для этих типов"
;;                                     (list op type-tags)))))))
;;               (error "Нет метода для этих типов"
;;                      (list op type-tags)))))))

;;; x82
;; Example - when dumb solution from SICP doesn't work:
;; rational and complex if there is no explicit coercion
;; rational->complex, but more advanced dispatch can use
;; rational->scheme-number and scheme-number->complex consequently.
;; Solution: see definition of procedure apply-generic

(load "./table.scm")

;;; Generic arithmetic operations
(define (add x . y) (apply-generic 'add (cons x y)))
(define (sub x . y) (apply-generic 'sub (cons x y)))
(define (mul x . y) (apply-generic 'mul (cons x y)))
(define (div x . y) (apply-generic 'div (cons x y)))

;;; Constructors
(define (make-scheme-number n) ((get proc-table 'make 'scheme-number) n))
(define (make-rational n d) ((get proc-table 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get proc-table 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get proc-table 'make-from-mag-ang 'complex) r a))

;;; Selectors
;; for complex nums
(define (real-part z) (apply-generic 'real-part (list z)))
(define (imag-part z) (apply-generic 'imag-part (list z)))
(define (magnitude z) (apply-generic 'magnitude (list z)))
(define (angle z) (apply-generic 'angle (list z)))
;; for rational nums
(define (numer p) (apply-generic 'numer (list p)))
(define (denom p) (apply-generic 'denom (list p)))

;;; Predicates
;; TODO: arbitrary arity
(define (equ? x y) (apply-generic 'equ? (list x y)))
(define (=zero? x) (apply-generic '=zero? (list x)))

;;; Tagging stuff
(define (attach-tag tag contents)
  (if (eq? tag 'scheme-number)
      contents
      (cons tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((and (pair? datum) (symbol? (car datum))) (car datum))
        (else (error "TYPE-TAG: Unexpected type of data"))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((and (pair? datum) (symbol? (car datum))) (cdr datum))
        (else (error "CONTENTS: Unexpected type of data"))))
(define (same-type? x . y)
  (define (iter type-tags)
    (cond ((null? (cdr type-tags)) #t)
          ((let ((t1 (car type-tags))
                 (t2 (cadr type-tags))) (eq? t1 t2))
           (iter (cdr type-tags)))
          (else #f)))
  (iter (map type-tag (cons x y))))

;;; Package installers
(define (install-scheme-number-package proc-table)
  (define (make x) x)
  ;; Adding procs to proc-table
  (put proc-table 'add '(scheme-number scheme-number) +)
  (put proc-table 'sub '(scheme-number scheme-number) -)
  (put proc-table 'mul '(scheme-number scheme-number) *)
  (put proc-table 'div '(scheme-number scheme-number) /)
  (put proc-table 'equ? '(scheme-number scheme-number) =)
  (put proc-table '=zero? '(scheme-number) zero?)
  (put proc-table 'make 'scheme-number make)
  ;; unary
  (put proc-table 'add '(scheme-number) identity)
  (put proc-table 'sub '(scheme-number) identity)
  (put proc-table 'mul '(scheme-number) identity)
  (put proc-table 'div '(scheme-number) identity)
  (put proc-table 'equ? '(scheme-number) (lambda (x) #t))
  proc-table)

(define (install-rational-package proc-table)
  (define (tag p) (attach-tag 'rational p))
  ;; Selectors
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; Constructor
  (define (make n d)
    (let ((g (gcd n d)))
      (tag (cons (/ n g) (/ d g)))))
  ;; Operations
  (define (add x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make (+ (* n q) (* p d))
        (* d q))))
  (define (sub x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make (- (* n q) (* p d))
        (* d q))))
  (define (mul x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make (* n p)
        (* d q))))
  (define (div x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (make (* n q)
        (* d p))))
  ;; Predicates
  (define (equ? x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (= (* n q) (* p d))))
  (define (=zero? x) (zero? (numer x)))
  ;; Adding procs to proc-table
  ;; In SICP tagging is when putting procs to table for all packages.
  (put proc-table 'add '(rational rational) add)
  (put proc-table 'sub '(rational rational) sub)
  (put proc-table 'mul '(rational rational) mul)
  (put proc-table 'div '(rational rational) div)
  (put proc-table 'equ? '(rational rational) equ?)
  (put proc-table '=zero? '(rational) =zero?)
  (put proc-table 'numer '(rational) numer)
  (put proc-table 'denom '(rational) denom)
  (put proc-table 'make 'rational make)
  ;; unary
  (put proc-table 'add '(rational) tag)
  (put proc-table 'sub '(rational) tag)
  (put proc-table 'mul '(rational) tag)
  (put proc-table 'div '(rational) tag)
  (put proc-table 'equ? '(rational) (lambda (p) #t))
  proc-table)

(define (install-rectangular-package proc-table)
  (define (tag x) (attach-tag 'rect x))
  ;; Selectors
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (let ((x (real-part z))
          (y (imag-part z)))
      (sqrt (+ (* x x) (* y y)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  ;; Constructors
  (define (make-from-real-imag x y) (tag (cons x y)))
  (define (make-from-mag-ang r a)
    (tag (cons (* r (cos a)) (* r (sin a)))))
  ;; Predicates
  (define (equ? z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2))))
  (define (=zero? z) (and (zero? (real-part z))
                          (zero? (imag-part z))))
  ;; Adding procs to proc-table
  (put proc-table 'equ? '(rect rect) equ?) ;x79
  (put proc-table '=zero? '(rect) =zero?)
  (put proc-table 'real-part '(rect) real-part)
  (put proc-table 'imag-part '(rect) imag-part)
  (put proc-table 'magnitude '(rect) magnitude)
  (put proc-table 'angle '(rect) angle)
  (put proc-table 'make-from-real-imag 'rect make-from-real-imag)
  (put proc-table 'make-from-mag-ang 'rect make-from-mag-ang)
  proc-table)

(define (install-polar-package proc-table)
  (define (tag x) (attach-tag 'polar x))
  ;; Selectors
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  ;; Constructors
  (define (make-from-real-imag x y)
    (tag (cons (sqrt (+ (* x x) (* y y))) (atan y x))))
  (define (make-from-mag-ang r a)
    (tag (cons r a)))
  ;; Predicates
  (define (equ? z1 z2) (and (= (magnitude z1) (magnitude z2))
                            (= (angle z1) (angle z2))))
  (define (=zero? z) (zero? (magnitude z)))
  ;; Adding procs to proc-table
  (put proc-table 'equ? '(polar polar) equ?) ;x79
  (put proc-table 'equ? '(polar polar) equ?) ;x79
  (put proc-table '=zero? '(polar) =zero?)
  (put proc-table 'real-part '(polar) real-part)
  (put proc-table 'imag-part '(polar) imag-part)
  (put proc-table 'magnitude '(polar) magnitude)
  (put proc-table 'angle '(polar) angle)
  (put proc-table 'make-from-real-imag 'polar make-from-real-imag)
  (put proc-table 'make-from-mag-ang 'polar make-from-mag-ang)
  proc-table)

(define (install-complex-package proc-table)
  (define (tag z) (attach-tag 'complex z))
  (define (make-from-real-imag x y)
    (tag ((get proc-table 'make-from-real-imag 'rect) x y)))
  (define (make-from-mag-ang r a)
    (tag ((get proc-table 'make-from-mag-ang 'polar) r a)))
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
  ;; Adding procs to proc-table
  (put proc-table 'add '(complex complex) add)
  (put proc-table 'sub '(complex complex) sub)
  (put proc-table 'mul '(complex complex) mul)
  (put proc-table 'div '(complex complex) div)
  (put proc-table 'equ? '(complex complex) equ?)
  (put proc-table '=zero? '(complex) =zero?)
  (put proc-table 'real-part '(complex) real-part)
  (put proc-table 'imag-part '(complex) imag-part)
  (put proc-table 'magnitude '(complex) magnitude)
  (put proc-table 'angle '(complex) angle)
  (put proc-table 'make-from-real-imag 'complex make-from-real-imag)
  (put proc-table 'make-from-mag-ang 'complex make-from-mag-ang)
  ;; unary
  (put proc-table 'add '(complex) tag)
  (put proc-table 'sub '(complex) tag)
  (put proc-table 'mul '(complex) tag)
  (put proc-table 'div '(complex) tag)
  (put proc-table 'equ? '(complex) (lambda (z) #t))
  proc-table)

(define (install-packs proc-table pack-installers)
  (if (null? pack-installers)
      proc-table
      (install-packs ((car pack-installers) proc-table)
                     (cdr pack-installers))))

;;; Table of procedures
(define proc-table (install-packs (make-table)
                                  (list install-scheme-number-package
                                        install-rational-package
                                        install-rectangular-package
                                        install-polar-package
                                        install-complex-package)))

;;; Coercion
(define (scheme-number->complex x) (make-complex-from-real-imag x 0))
(define (rational->scheme-number x)
  (make-scheme-number (/ (numer x) (denom x))))

(define coercion-table (make-table))

(put coercion-table 'complex 'scheme-number scheme-number->complex)
(put coercion-table 'scheme-number 'rational rational->scheme-number)

;; coercion: type1->type2
(define (get-coercion t1 t2) (get coercion-table t2 t1))

;;; x82. Dispatch
;; This implementation doesn't expect cycles in graph of types.
(define (apply-generic op args)
  (define (flatmap proc lst) (apply append (map proc lst)))
  (define (result args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get proc-table op type-tags)))
        (if proc
            (apply proc (map contents args))
            #f))))
  (define (available-coercion x)
    (let ((type (type-tag x)))
      (let ((coer-procs (get-column coercion-table type)))
        (if coer-procs
            (cons x (flatmap available-coercion
                             (map (lambda (proc) (proc x)) coer-procs)))
            (list x)))))
  (define (same-type-filter list-of-arg-lists)
    (filter (lambda (arg-list) (apply same-type? arg-list))
            list-of-arg-lists))
  (define (lookup-res list-of-arg-lists)
    (if (null? list-of-arg-lists)
        (error "APPLY-GENERIC: No method for this types" (cons op args))
        (let ((res (result (car list-of-arg-lists))))
          (if res res (lookup-res (cdr list-of-arg-lists))))))
  (let ((len (length args))
        (res (result args)))
    (cond ((= len 0) (error "APPLY-GENERIC: No args"))
          ((= len 1)
           (if res
               res
               (lookup-res (map list (available-coercion (car args))))))
          ((= len 2)
           (if res
               res
               (let ((a1 (car args))
                     (a2 (cadr args)))
                 (lookup-res
                  (same-type-filter
                   (flatmap (lambda (x2)
                              (map (lambda (x1) (list x1 x2))
                                   (available-coercion a1)))
                            (available-coercion a2)))))))
          (else
           (apply-generic op (cons (apply-generic op (list (car args)
                                                           (cadr args)))
                                   (cddr args)))))))

;;; Testing stuff
;; keys should be different
(define keys (map (lambda (key) (make-key key))
                  '(scheme-number
                    (scheme-number)
                    (scheme-number scheme-number)
                    rational
                    (rational)
                    (rational rational)
                    rect
                    (rect)
                    (rect rect)
                    polar
                    (polar)
                    (polar polar)
                    complex
                    (complex)
                    (complex complex))))

(define (print-nums nums)
  (for-each (lambda (n) (display n) (newline)) nums))

(define scheme-nums (map (lambda (n) (make-scheme-number n))
                         '(0
                           1
                           -1
                           2
                           -4
                           -3
                           3.5
                           5)))
(define n0 (car scheme-nums))
(define n1 (cadr scheme-nums))
(define scheme-nums2 (map (lambda (n) (sub (add (div (mul n n) n) n) n))
                          (cdr scheme-nums)))
(define scheme-nums3 (map (lambda (n) (add (div n n1) n0))
                          scheme-nums))
(define scheme-nums4 (map (lambda (n) (div n n)) (cdr scheme-nums)))

(define rat-nums (map (lambda (pair)
                        (make-rational (car pair) (cadr pair)))
                      '((0 1)
                        (1 1)
                        (1 -1)
                        (-1 1)
                        (1 2)
                        (-1 -4)
                        (-3 5)
                        (2 3))))
(define p0 (car rat-nums))
(define p1 (cadr rat-nums))
(define rat-nums2 (map (lambda (p) (sub (add (div (mul p p) p) p) p))
                       (cdr rat-nums)))
(define rat-nums3 (map (lambda (p) (add (div p p1) p0))
                       (cdr rat-nums)))
(define rat-nums4 (map (lambda (p) (div p p))
                       (cdr rat-nums)))

(define complex-nums (map (lambda (pair)
                            (make-complex-from-real-imag (car pair)
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
(define complex-nums4 (map (lambda (z) (div z z)) (cdr complex-nums)))
