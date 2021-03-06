(define (element-of-set? x set)
  (cond ((or (null? set) (< x (car set))) #f)
        ((= x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; x61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; steps(n) = O(n)
(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((< x1 x2) (intersection-set (cdr s1) s2))
              ((> x1 x2) (intersection-set s1 (cdr s2)))
              (else (cons x1 (intersection-set (cdr s1) (cdr s2))))))))

;; x62
;; steps(n) = O(n)
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1))
                    (x2 (car s2)))
                (cond ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                      ((> x1 x2) (cons x2 (union-set s1 (cdr s2))))
                      (else (cons x1 (union-set (cdr s1) (cdr s2)))))))))
