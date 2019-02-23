(define (my-list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (my-length-recur items)
  (if (null? items)
      0
      (+ 1 (my-length-recur (cdr items)))))

(define (my-length items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (+ acc 1))))
  (iter items 0))

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append (cdr list1) list2))))


;;; x17

(define (my-last-pair items)
  (if (null? (cdr items))
      (car items)
      (my-last-pair (cdr items))))

;; double-pass: list-ref and length
(define (my-last-pair2 items)
  (my-list-ref items (- (my-length items) 1)))


;;; x18

(define (my-reverse items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (cons (car items) acc))))
  (iter items '()))


;;; x19 (see 1.2.2)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; The counting algorithm doesn't use order of values of coins.
(define us-coins-random (list 10 1 50 25 5))

(define (cc amount coin-values)
  (let ((no-more? null?)
        (first-denomination car)
        (except-first-denomination cdr))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc (- amount (first-denomination coin-values))
                       coin-values)
                   (cc amount
                       (except-first-denomination coin-values)))))))

;; Optional: iterative implementation


;;; x20

(define (same-parity a . b)
  (define (parity n)
    (if (even? n)
        2
        1))
  (define (same-parity? n) (= (parity a) (parity n)))
  (define (recur b)
    (cond ((null? b) '())
          ((same-parity? (car b)) (cons (car b) (recur (cdr b))))
          (else (recur (cdr b)))))
  (cons a (recur b)))


;;; x21

(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (my-map proc (cdr items)))))

(define (scale-list items factor)
  (my-map (lambda (x) (* x factor)) items))

(define (scale-list2 items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (square-list items)
  (my-map square items))

(define (square-list2 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list2 (cdr items)))))

(define (square x) (* x x))


;;; x22

;; cons appends item at the beginning of a list
(define (square-list-iter-doom items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (cons (square (car items)) acc))))
  (iter items '()))

;; appending at the end by cons produces not a list
(define (square-list-iter-doom2 items)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (cons acc (square (car items))))))
  (iter items '()))


;;; x23

(define (my-for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (my-for-each proc (cdr items)))))


;;; Optional
;; Is it necessary to do an iterative implementation for map and same
;; list things? More productive?

(define (repeat-proc proc args times)
  (cond ((= times 0))
        (else (apply proc args)
              (repeat-proc proc args (- times 1)))))

;; ,time (repeat-proc square-list (list us-coins) 100000)

;; Time test doesn't confirm that square-list-iter-doom is faster than
;; recursive implementation.
