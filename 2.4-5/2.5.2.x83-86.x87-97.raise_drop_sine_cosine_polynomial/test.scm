(load "./install.scm")

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
;; Why are P1 and P1~ not equal? (equal with pseudodivision)

(newline)
(if (and (equ? 1.0 (apply mul scheme-nums4))
         (equ? 1.0 (apply mul rat-nums4))
         (equ? 1.0 (apply mul complex-nums4))
         (equ? P1 P1~))
    (display "TEST PASSED")
    (display "TEST FAILED"))
(newline)
