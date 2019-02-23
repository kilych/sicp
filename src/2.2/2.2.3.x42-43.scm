;; enumerate-interval in SICP
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (zeros-seq n)
  (if (= n 0)
      '()
      (cons 0 (zeros-seq (- n 1)))))


;; I made a mistake in Hugo's way (see x43).
(define (queens-bad n)
  (define (positions cols)
    (if (= cols 1)
        (map list (enumerate 1 n))
        (flatmap (lambda (row)
                   (map (lambda (pos) (cons row pos))
                        (filter (lambda (pos) (safe? row pos))
                                (positions (- cols 1)))))
                 (enumerate 1 n))))
  (positions n))

(define (queens-bad2 n)
  (define (positions cols)
    (display "---")
    (newline)
    (if (= cols 1)
        (map list (enumerate 1 n))
        (filter (lambda (pos) (safe? (car pos) (cdr pos)))
                (flatmap (lambda (row)
                           (newline)
                           (map (lambda (pos)
                                  (display row)
                                  (display " ")
                                  (cons row pos))
                                (positions (- cols 1))))
                         (enumerate 1 n)))))
  (positions n))

;; Here and above cols from right to left, it is for using cons.
;; empty board is ()
;; "VM: Stack overflow" for (queens 10)
;; Can not put the filter in deep like in queens-bad because then row
;; is unbound in the filter.
(define (queens n)
  (define (positions cols)
    (display "---")
    (newline)
    (if (= cols 1)
        (map list (enumerate 1 n))
        (filter (lambda (pos) (safe? (car pos) (cdr pos)))
                (flatmap (lambda (pos)
                           (newline)
                           (map (lambda (row)
                                  (display row)
                                  (display " ")
                                  (cons row pos))
                                (enumerate 1 n)))
                         (positions (- cols 1))))))
  (positions n))

(define (safe? row pos)
  (define (iter k pos)
    (cond ((null? pos) #t)
          ((let ((first (car pos)))
             (or (= row first)
                 (= (+ row k) first)
                 (= (- row k) first)))
           #f)
          (else (iter (+ k 1) (cdr pos)))))
  (iter 1 pos))


;;; x42

;; k numbers from left to right
;; empty board is (0 0 .. 0 0) according to board size
;; It is in two times slower than mine.
(define (queens-sicp board-size)
  (define empty-board (zeros-seq board-size))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe-sicp? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-pos new-row k rest-of-queens))
                                (enumerate 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-pos new-row k rest-of-queens)
  (if (= k 1)
      (cons new-row (cdr rest-of-queens))
      (cons (car rest-of-queens)
            (adjoin-pos new-row (- k 1) (cdr rest-of-queens)))))

(define (safe-sicp? k positions)
  (define row (list-ref positions (- k 1)))
  (define (iter n pos)
    (let ((first (car pos)))
      (cond ((= n 0) #t)
            ((or (= row first)
                 (= (+ row n) first)
                 (= (- row n) first)) #f)
            (else (iter (- n 1) (cdr pos))))))
  (iter (- k 1) positions))


;;; x43

(define (queens-doom board-size)
  (define empty-board (zeros-seq board-size))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe-sicp? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-pos new-row
                                              k
                                              rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate 1 board-size)))))
  (queen-cols board-size))

;; If time of (queens-sicp 8) is T, what is time of (queens-doom 8)?
;; At first, queens-doom produces tree recursive process instead of
;; linear recursive process.
;; If T[(queens-sicp 3)] ~= T[(queens-doom 3)] then (queens-sicp 8)
;; faster than (queens-doom 8) about 3000 ~= 5^(8-3) = 5^5 times
;; (5 subject to filter) because (queens-doom 8) runs (queens-cols 3)
;; this number of times.
