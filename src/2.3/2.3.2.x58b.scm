;; For the first time first and rest for addition and multiplication
;; are different, but is not necessary, I think.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp)
                                                       var))
                                  (make-product (deriv (multiplier exp)
                                                       var)
                                                (multiplicand exp))))
        (else (error "Unexpected expression:" exp))))

(define (var? x) (symbol? x))

(define (same-var? x y) (and (var? x) (var? y) (eq? x y)))

(define (sum? exp) (and (pair? exp) (memq '+ exp)))

(define (product? exp) (and (pair? exp) (memq '* exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (append (list-if-not a1) (cons '+ (list-if-not a2))))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (append (list-if-not m1) (list '* m2)))))

;; eq? can do it too
(define (=number? n1 n2)
  (and (number? n1) (number? n2) (= n1 n2)))

(define (list-if-not x)
  (if (pair? x)
      x
      (list x)))

(define (addend sum) (first '+ sum))

(define (augend sum) (rest '+ sum))

(define (multiplier prod) (first '* prod))

(define (multiplicand prod) (rest '* prod))

(define (first op exp)
  (define (recur exp)
    (if (eq? op (car exp))
        '()
        (cons (car exp) (recur (cdr exp)))))
  (let ((f (recur exp)))
    (if (null? (cdr f))
        (car f)
        f)))

(define (rest op exp)
  (let ((r (cdr (memq op exp))))
    (if (null? (cdr r))
        (car r)
        r)))
