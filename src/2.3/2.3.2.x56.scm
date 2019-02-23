(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (first exp) var)
                              (deriv (rest exp) var)))
        ((product? exp) (make-sum (make-product (deriv (first exp) var)
                                                (rest exp))
                                  (make-product (first exp)
                                                (deriv (rest exp) var))))
        ;; x56
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1))))
        (else (error "Unexpected expression:" exp))))

(define (var? x) (symbol? x))

(define (same-var? x y) (and (var? x) (var? y) (eq? x y)))

(define (sum? exp) (and (pair? exp) (eq? '+ (op exp))))

(define (product? exp) (and (pair? exp) (eq? '* (op exp))))

(define (make-sum addend augend)
  (cond ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        (else (list '+ addend augend))))

(define (make-product multiplier multiplicand)
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier) (number? multiplicand))
         (* multiplier multiplicand))
        (else (list '* multiplier multiplicand))))

;; eq? can do it too
(define (=number? n1 n2)
  (and (number? n1) (number? n2) (= n1 n2)))

(define (op exp) (car exp))

(define (first exp) (cadr exp))

(define (rest exp) (caddr exp))


;;; x56

(define (exponentiation? exp) (and (pair? exp) (eq? '** (car exp))))

(define (base exp) (first exp))

(define (exponent exp) (rest exp))

(define (make-exponentiation base exponent)
  (if (number? exponent)
      (cond ((= exponent 0) 1)
            ((= exponent 1) base)
            (else (list '** base exponent)))
      (error "Unexpected exponent:" exponent)))
