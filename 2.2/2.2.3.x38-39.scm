;;; x38

;; accumulate (also known as fold-right)
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;; In addition to fold-right procedure fold-left exists.
(define (fold-left op initial sequence)
  (define (iter acc seq)
    (if (null? seq)
        acc
        (iter (op acc (car seq)) (cdr seq))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;; => 3/2

(fold-left / 1 (list 1 2 3))
;; => 1/6

(fold-right list '() (list 1 2 3))
;; => (1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
;; => (((() 1) 2) 3)

;; If op is commutative, fold procedures return the same result.


;;; x39

(define (my-reverse seq)
  (fold-right (lambda (first rest) (append rest (list first)))
              '()
              seq))

(define (my-reverse2 seq)
  (fold-left (lambda (acc first) (append (list first) acc))
             '()
             seq))
