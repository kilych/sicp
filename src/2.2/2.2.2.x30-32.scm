(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; We can stop thinking about end of list - '().
(define (scale-tree2 tree factor)
  (map (lambda (subtree)
         (cond ((null? subtree) '())
               ((not (pair? subtree)) (* subtree factor))
               (else (scale-tree2 subtree factor))))
       tree))


;;; x30

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (subtree)
         (cond ((null? subtree) '())
               ((not (pair? subtree)) (square subtree))
               (else (square-tree2 subtree))))
       tree))


;;; x31

(define (square-tree3 tree)
  (tree-map square tree))

(define (square-tree4 tree)
  (tree-map2 square tree))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (tree-map2 proc tree)
  (map (lambda (subtree)
         (cond ((null? subtree) '())
               ((not (pair? subtree)) (proc subtree))
               (else (tree-map2 proc subtree))))
       tree))


;;; x32

(define (subsets set)
  (define (iter set acc)
    (if (null? set)
        acc
        (let ((new-acc (append acc (map (lambda (subset)
                                          (cons (car set) subset))
                                        acc))))
          (iter (cdr set) new-acc))))
  (iter set '(())))

(define (subsets-recur set)
  (if (null? set)
      '(())
      (let ((rest (subsets-recur (cdr set))))
        (append rest (map (lambda (subset) (cons (car set) subset))
                          rest)))))
