(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter n)
  (define (iter a b count)
    (if (= count n)
        a
        (iter b (+ a b) (+ count 1))))
  (iter 0 1 0))


(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


;; Optional.
;; Iterative process implementation for count-change. Tree traversal,
;; but tree is tree of computation instead tree of data.
(define (count-change-iter init-amount)
  (define max-kinds-of-coins 5)
  (define (iter kinds
                amount
                saved-amounts
                accum)
    (define (add)
      (if (= (remainder amount (first-denomination kinds))
             0)
          1
          0))
    (cond ((and (= kinds 5)
                (<= amount 0)) (+ accum (add)))
          ((or (= kinds 1)
               (<= amount 0)) (iter (+ kinds 1)
                                    (- (car saved-amounts)
                                       (first-denomination (+ kinds 1)))
                                    (cdr saved-amounts)
                                    (+ accum (add))))
          (else (iter (- kinds 1)
                      amount
                      (cons amount saved-amounts)
                      accum))))
  (iter max-kinds-of-coins
        init-amount
        '()
        0))
