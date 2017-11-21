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
