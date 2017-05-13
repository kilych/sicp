(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (first exp) var)
                              (deriv (rest exp) var)))
        ((product? exp) (make-sum (make-product (first exp)
                                                (deriv (rest exp) var))
                                  (make-product (deriv (first exp) var)
                                                (rest exp))))
        (else (error "Unexpected expression:" exp))))

(define (var? x) (symbol? x))

(define (same-var? x y) (and (var? x) (var? y) (eq? x y)))

(define (sum? exp) (and (pair? exp) (eq? '+ (op exp))))

(define (product? exp) (and (pair? exp) (eq? '* (op exp))))

(define (make-sum addend augend)
  (cond ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        (else (list addend '+ augend))))

(define (make-product multiplier multiplicand)
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier) (number? multiplicand))
         (* multiplier multiplicand))
        (else (list multiplier '* multiplicand))))

;; eq? can do it too
(define (=number? n1 n2)
  (and (number? n1) (number? n2) (= n1 n2)))

(define (op exp) (cadr exp))

(define (first exp) (car exp))

(define (rest exp) (caddr exp))
