;;; x77
;; Substitution:
;; (magnitude (complex rect 3 . 4))
;; (apply-generic magnitude (complex rect 3 . 4))
;; (apply magnitude ((rect 3 . 4)))
;; (magnitude (rect 3 . 4))
;; (apply-generic magnitude (rect 3 . 4))
;; (apply magnitude ((3 . 4)))             ;rectangular implementation
;; (magnitude (3 . 4))                     ;rectangular implementation
;; (sqrt (+ (* 3 3) (* 4 4)))
;; 5
;; We called apply-generic two times. apply-generic called two
;; different procedures from different columns of proc-table.

;;; Generic arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; Constructors
(define (make-scheme-num n) ((get-proc proc-table 'make 'scheme-num) n))
(define (make-rational n d) ((get-proc proc-table 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get-proc proc-table 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get-proc proc-table 'make-from-mag-ang 'complex) r a))

;;; Selectors (for complex nums)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; Predicates
;;; x79
(define (equ? x y) (apply-generic 'equ? x y))
;;; x80
(define (=zero? x) (apply-generic '=zero? x))

;;; x78
;;; Tagging stuff
(define (attach-tag tag contents)
  (if (eq? tag 'scheme-num)
      contents
      (cons tag contents)))
(define (type-tag datum)
  (if (number? datum)
      'scheme-num
      (car datum)))
(define (contents datum)
  (if (number? datum)
      datum
      (cdr datum)))

;;; Package installers
(define (install-scheme-num-package proc-table)
  (define (tag x) (attach-tag 'scheme-num x))
  (define add (lambda (x y) (tag (+ x y))))
  (define sub (lambda (x y) (tag (- x y))))
  (define mul (lambda (x y) (tag (* x y))))
  (define div (lambda (x y) (tag (/ x y))))
  (define make (lambda (x) (tag x)))
  (add-col proc-table (list 'scheme-num
                            add
                            sub
                            mul
                            div
                            make
                            #f
                            #f
                            =           ;x79
                            zero?       ;x80
                            #f
                            #f
                            #f
                            #f)))

(define (install-rational-package proc-table)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (attach-tag 'rational (cons (/ n g) (/ d g)))))
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
  (define (equ?-rat x y)
    (let ((n (numer x))
          (d (denom x))
          (p (numer y))
          (q (denom y)))
      (= (* n q) (* p d))))
  (define (=zero?-rat x) (zero? (numer x)))
  (add-col proc-table (list 'rational
                            add-rat
                            sub-rat
                            mul-rat
                            div-rat
                            make-rat
                            #f
                            #f
                            equ?-rat    ;x79
                            =zero?-rat  ;x80
                            #f
                            #f
                            #f
                            #f)))

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
  (define (equ?-rect z1 z2) (and (= (real-part z1) (real-part z2))
                                 (= (imag-part z1) (imag-part z2))))
  (define (=zero?-rect z) (and (zero? (real-part z))
                               (zero? (imag-part z))))
  (add-col proc-table (list 'rect
                            #f          ;add
                            #f          ;sub
                            #f          ;mul
                            #f          ;div
                            #f          ;make
                            make-from-real-imag
                            make-from-mag-ang
                            equ?-rect   ;x79
                            =zero?-rect ;x80
                            real-part
                            imag-part
                            magnitude
                            angle)))

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
  (define (equ?-polar z1 z2) (and (= (magnitude z1) (magnitude z2))
                                  (= (angle z1) (angle z2))))
  (define (=zero?-polar z) (zero? (magnitude z)))
  (add-col proc-table (list 'polar
                            #f          ;add
                            #f          ;sub
                            #f          ;mul
                            #f          ;div
                            #f          ;make
                            make-from-real-imag
                            make-from-mag-ang
                            equ?-polar   ;x79
                            =zero?-polar ;x80
                            real-part
                            imag-part
                            magnitude
                            angle)))

(define (install-complex-package proc-table)
  (define (make-from-real-imag x y)
    ((get-proc proc-table 'make-from-real-imag 'rect) x y))
  (define (make-from-mag-ang r a)
    ((get-proc proc-table 'make-from-mag-ang 'polar) r a))
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
  ;; In SICP tagging is when putting procs to table for all packages.
  (define (tag z) (attach-tag 'complex z))
  ;; Mapping works because all procs have the same arity.
  (add-col proc-table (cons 'complex
                            (append (map (lambda (proc)
                                           (if proc
                                               (lambda (arg1 arg2)
                                                 (tag (proc arg1 arg2)))
                                               #f))
                                         (list add
                                               sub
                                               mul
                                               div
                                               #f ;make
                                               make-from-real-imag
                                               make-from-mag-ang))
                                    ;; x77
                                    (list equ?   ;x79
                                          =zero? ;x80
                                          real-part
                                          imag-part
                                          magnitude
                                          angle)))))

(define (install-packs proc-table pack-installers)
  (if (null? pack-installers)
      proc-table
      (install-packs ((car pack-installers) proc-table)
                     (cdr pack-installers))))

;;; Table stuff
;; Table is a list of rows. Row is a list of cells.
(define (add-col table col)
  (if (null? table)
      (map list col)
      (map (lambda (cell row) (cons (car row) (cons cell (cdr row))))
           col
           table)))

(define (print-table table)
  (for-each (lambda (row) (display row) (newline))
            table))

(define proc-table (install-packs (add-col '() '(op
                                       add
                                       sub
                                       mul
                                       div
                                       make
                                       make-from-real-imag
                                       make-from-mag-ang
                                       equ?   ;x79
                                       =zero? ;x80
                                       real-part
                                       imag-part
                                       magnitude
                                       angle))
                                  (list install-scheme-num-package
                                        install-rational-package
                                        install-rectangular-package
                                        install-polar-package
                                        install-complex-package)))

(define (get-proc proc-table op type)
  (define (find-row op table)
    (cond ((null? table) #f)
          ((eq? op (caar table)) (car table))
          (else (find-row op (cdr table)))))
  (define (find-ref item lst)
    (define (iter ref lst)
      (cond ((null? lst) #f)
            ((equal? item (car lst)) ref)
            (else (iter (+ ref 1) (cdr lst)))))
    (iter 0 lst))
  (let ((row (find-row op (cdr proc-table)))
        (col (find-ref type (car proc-table))))
    (if (and row col)
        (list-ref row col)
        #f)))

(define (apply-generic op . args)
  (define (same-type? args)
    (cond ((or (null? args) (null? (cdr args))) #t)
          ((eq? (type-tag (car args)) (type-tag (cadr args)))
           (same-type? (cdr args)))
          (else #f)))
  (if (same-type? args)
      (let ((type (type-tag (car args))))
        (let ((proc (get-proc proc-table op type)))
          (if proc
              (apply proc (map contents args))
              (error "APPLY-GENERIC: Operator for this type not found in proc-table:"
                     (list op type)))))
      (error "APPLY-GENERIC: args have different types." args)))

;;; Testing stuff
(define (print-nums nums)
  (for-each (lambda (n) (display n) (newline)) nums))

(define scheme-nums (map (lambda (n) (make-scheme-num n))
                         '(0
                           1
                           -1
                           2
                           -4
                           -3
                           3.5)))
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
                        (1 -1)
                        (-1 1)
                        (1 2)
                        (-1 -4)
                        (-3 5))))
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
(define complex-nums4 (map (lambda (z) (div z z))
                           (cdr complex-nums)))
