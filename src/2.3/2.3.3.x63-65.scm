(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))
        (else #t)))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))
        (else set)))

(define (tree? x)
  (and (not (null? x))
       (not (pair? (car x)))
       (not (null? (cdr x)))
       (or (null? (cadr x)) (pair? (cadr x)))
       (not (null? (cddr x)))
       (or (null? (caddr x)) (pair? (caddr x)))
       (null? (cdddr x))))

(define (manual-tree items)
  (define (iter items tree)
    (if (null? items)
        tree
        (iter (cdr items) (adjoin-set (car items) tree))))
  (iter items '()))

(define (random-tree size)
  (define (iter k tree)
    (if (= k 0)
        tree
        (iter (- k 1) (adjoin-set (random size) tree))))
  (iter size '()))

(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ low 1) high))))


;;; x63

;; Tree recursion: steps(n) = O(n * log(n))
;; Double work: first, it cons every item to lists, second, it appends
;; lists. For tree with empty right branches steps = n * n/2
;; (arithmetical progression).
;; For balanced tree it appends n/2 for log(n) times. For every
;; "level" of recursion it appends half items to other half.
(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left-branch tree))
              (cons (entry tree) (tree->list1 (right-branch tree))))))

;; Linear recursion: steps(n) = O(n)
;; Variable-length stack of deferred calls: "forward" - "backward".
;; Interesting approach (maybe for count-change).
(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Substitution:
;; (tree->list2 '(2 (1 () ()) (3 () ())))
;; (copy-to-list '(2 (1 () ()) (3 () ())) '())
;; (copy-to-list '(1 () ()) (cons 2 (copy-to-list '(3 () ()) '())))
;; (copy-to-list '(1 () ()) (cons 2 (copy-to-list '() (cons 3 (copy-to-list '() '())))))
;; (copy-to-list '(1 () ()) (cons 2 (copy-to-list '() (cons 3 '()))))
;; (copy-to-list '(1 () ()) (cons 2 (copy-to-list '() '(3))))
;; (copy-to-list '(1 () ()) (cons 2 '(3)))
;; (copy-to-list '(1 () ()) '(2 3))
;; (copy-to-list '() (cons 1 (copy-to-list '() '(2 3))))
;; (copy-to-list '() (cons 1 '(2 3)))
;; (copy-to-list '() '(1 2 3))
;; '(1 2 3)

;; x63a
;; (tree->list1 (manual-tree '(7 3 9 1 5 11)))
;; (tree->list2 (manual-tree '(7 3 9 1 5 11)))
;; (tree->list1 (manual-tree '(3 1 7 5 9 11)))
;; (tree->list2 (manual-tree '(3 1 7 5 9 11)))
;; (tree->list1 (manual-tree '(5 3 9 1 7 11)))
;; (tree->list2 (manual-tree '(5 3 9 1 7 11)))
;; => (1 3 5 7 9 11)


;;; x64

;; steps(n) = O(n*log(n)): O(n) for mid-split and O(log(n)) for
;; list->tree itself
(define (list->tree items)
  (if (null? items)
      '()
      (let ((branches (mid-split items)))
        (let ((left (car branches))
              (entr (cadr branches))
              (right (cddr branches)))
          (make-tree entr (list->tree left) (list->tree right))))))

(define (mid-split items)
  (define (iter reversed-head rest items)
    (if (or (null? items) (null? (cdr items)))
        (cons (reverse reversed-head) rest)
        (iter (cons (car rest) reversed-head)
              (cdr rest)
              (cddr items))))
  (iter '() items items))

;; from sicp
(define (sicp-list->tree elts)
  (car (partial-tree elts (length elts))))

;; like copy-to-list: going to leftmost branch, then right-branch at
;; the same level, then level up
;; steps(n) = O(n)
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n 1 left-size)))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(sicp-list->tree '(1 3 5 7 9 11))
;; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))


;; x65

;; steps(n) = O(n)
(define (intersection-lst l1 l2)
  (cond ((or (null? l1) (null? l2)) '())
        ((> (car l1) (car l2)) (intersection-lst l1 (cdr l2)))
        ((< (car l1) (car l2)) (intersection-lst (cdr l1) l2))
        (else (cons (car l1) (intersection-lst (cdr l1) (cdr l2))))))

;; steps(n) = O(n)
(define (union-lst s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1))
                    (x2 (car s2)))
                (cond ((< x1 x2) (cons x1 (union-lst (cdr s1) s2)))
                      ((> x1 x2) (cons x2 (union-lst s1 (cdr s2))))
                      (else (cons x1 (union-lst (cdr s1) (cdr s2)))))))))

;; steps(n) = O(n)
(define (intersection-set t1 t2)
  (sicp-list->tree (intersection-lst (tree->list2 t1)
                                     (tree->list2 t2))))

;; steps(n) = O(n)
(define (union-set t1 t2)
  (sicp-list->tree (union-lst (tree->list2 t1) (tree->list2 t2))))


;;; Optional (see 1.2.2 and x2.19 in 2.2.1)

;; "iterative-recursive" approach like in copy-to-list and
;; partial-tree for count-change
(define (count-change amount coin-values)
  (define (cc amount coin-values accum)
    (cond ((null? coin-values) accum)
          ((null? (cdr coin-values))
           (if (= 0 (remainder amount (car coin-values)))
               (+ accum 1)
               accum))
          ((= 0 amount) (+ accum 1))
          ((> 0 amount) accum)
          (else (cc (- amount (car coin-values))
                    coin-values
                    (cc amount (cdr coin-values) accum)))))
  (cc amount coin-values 0))

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))
