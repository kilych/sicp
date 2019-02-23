;; Sets not multisets: every element is unique.

;; steps(n) = O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; steps(n) = O(1)
(define (adjoin-set x set) (cons x set))

;; steps(n) = O(n^2)
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

;; steps(n) = O(n^2)
;; If we don't check then O(n).
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (union-set (cdr s1) (cons (car s1) s2)))))
