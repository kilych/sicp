(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree)
                                (square tree)
                                0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (square x) (* x x))

(define (even-fibs n)
  (define (recur k term next)
    (if (> k n)
        '()
        (if (even? term)
            (cons term (recur (+ k 1) next (+ term next)))
            (recur (+ k 1) next (+ term next)))))
  (recur 0 0 1))

(define (fib n)
  (define (iter a b k)
    (if (= k n)
        a
        (iter b (+ a b) (+ k 1))))
  (iter 0 1 0))


;; Tries input sequence as flat sequence.
(define (my-filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (my-filter predicate (cdr sequence))))
        (else (my-filter predicate (cdr sequence)))))

;; It "predicates" (checks) atoms only. Tries all as tree.
(define (filter-tree check-atom tree)
  (cond ((null? tree) '())
        ((not (pair? (car tree)))
         (if (check-atom (car tree))
             (cons (car tree)
                   (filter-tree check-atom (cdr tree)))
             (filter-tree check-atom (cdr tree))))
        (else (cons (filter-tree check-atom (car tree))
                    (filter-tree check-atom (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; enumerate-interval in SICP
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))

;; also "enumerate-tree" in SICP
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define (sum-odd-squares2 tree)
  (accumulate +
              0
              (map square
                   (my-filter odd?
                              (fringe tree)))))

(define (even-fibs2 n)
  (accumulate cons
              '()
              (my-filter even? (map fib
                                    (enumerate 0 n)))))


;;; x33

(define (my-map proc items)
  (accumulate (lambda (item rest) (cons (proc item) rest))
              '()
              items))

(define (my-append lst1 lst2)
  (accumulate cons
              lst2
              lst1))

(define (my-length items)
  (accumulate (lambda (item rest) (+ 1 rest))
              0
              items))


;;; x34

(define (horners-rule value-of-x coefficients)
  (accumulate (lambda (this-coefficient rest)
                (+ this-coefficient (* rest value-of-x)))
              0
              coefficients))


;;; x35

(define (count-leaves tree)
  (accumulate (lambda (subtree rest)
                (if (not (pair? subtree))
                    (+ 1 rest)
                    (+ (count-leaves subtree) rest)))
              0
              tree))

(define (count-leaves2 tree)
  (accumulate +
              0
              (map (lambda (subtree)
                     (if (not (pair? subtree))
                         1
                         (count-leaves2 subtree)))
                   tree)))

(define (count-leaves3 tree)
  (accumulate (lambda (item rest) (+ 1 rest))
              0
              (fringe tree)))
