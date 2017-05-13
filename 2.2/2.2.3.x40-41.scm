(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; enumerate-interval in SICP
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum-pair?
                  (accumulate append
                              '()
                              (map (lambda (i)
                                     (map (lambda (j) (list j i))
                                          (enumerate 1 (- i 1))))
                                   (enumerate 2 n))))))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum-pair?
                  (flatmap (lambda (i)
                             (map (lambda (j) (list j i))
                                  (enumerate 1 (- i 1))))
                           (enumerate 2 n)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pair? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))


;; SICP:
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x) (map (lambda (p) (cons x p))
                                (permutations (remove-item x s))))
               s)))

(define (remove-item item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (permutations2 set)
  (define (put-item item head tail acc)
    (if (null? tail)
        (cons (append head (list item)) acc)
        (put-item item
                  (append head (list (car tail)))
                  (cdr tail)
                  (cons (append head (list item) tail) acc))))
  (define (iter set acc)
    (if (null? set)
        acc
        (iter (cdr set) (flatmap (lambda (permutation)
                                   (put-item (car set)
                                             '()
                                             permutation
                                             '()))
                                 acc))))
  (iter (cdr set) (list (list (car set)))))

;; Permutations of order of elements of set:
(define (permutations3 cardinality)
  (permutations (enumerate 1 cardinality)))


;;; x40

(define (prime-sum-pairs3 n)
  (map make-pair-sum
       (filter prime-sum-pair?
                  (unique-pairs n))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate 1 (- i 1))))
           (enumerate 2 n)))


;;; x41

(define (given-sum-threes n s)
  (define (given-sum? three)
    (= s (accumulate + 0 three)))
  (filter given-sum?
             (unique-threes n)))

(define (unique-threes n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list k j i))
                             (enumerate 1 (- j 1))))
                  (enumerate 2 (- i 1))))
           (enumerate 3 n)))

;; Optional:
;; procedure for making of arbitrary long ordered sequences of numbers.
