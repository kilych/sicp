(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


;;; x24

;; (list 1 (list 2 (list 3 4)))
;;  => (1 (2 (3 4)))


;;; x25

;; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;; (car (car '((7))))
;; (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))


;;; x26

(define x (list 1 2 3))
(define y (list 4 5 6))

;; (append x y)
;; => (1 2 3 4 5 6)

;; (cons x y)
;; => ((1 2 3) 4 5 6)

;; (list x y)
;; => ((1 2 3) (4 5 6))


;;; x27

(define (deep-reverse tree)
  (define (iter tree acc)
    (cond ((null? tree) acc)
          ((not (pair? tree)) tree)
          (else (iter (cdr tree) (cons (iter (car tree) '())
                                       acc)))))
  (iter tree '()))


;;; x28

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

;; not so iterative
(define (fringe-iter tree)
  (define (iter tree acc)
    (cond ((null? tree) acc)
          ((not (pair? tree)) (cons tree acc))
          (else (iter (car tree) (append (iter (cdr tree) '())
                                         acc)))))
  (iter tree '()))
