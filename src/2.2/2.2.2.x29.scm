(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))


(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


(define (total-weight mobile)
  (define (atom? x) (and (not (null? x)) (not (pair? x))))
  (if (atom? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))


(define (balanced? mobile)
  (define (atom? x) (and (not (null? x)) (not (pair? x))))
  (if (atom? mobile)
      #t
      (and (= (* (total-weight (branch-structure (left-branch mobile)))
                 (branch-length (left-branch mobile)))
              (* (total-weight (branch-structure (right-branch mobile)))
                 (branch-length (right-branch mobile))))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;; (balanced? '((1 5) (1 ((1 4) (4 1)))))
;; => #t


(define branch (make-branch 1 5))
(define mob (make-mobile branch branch))


;; Other implementation

;; (define (make-mobile left right)
;;   (cons left right))

;; (define (left-branch mobile)
;;   (car mobile))

;; (define (right-branch mobile)
;;   (cdr mobile))

;; (define (make-branch length structure)
;;   (cons length structure))

;; (define (branch-length branch)
;;   (car branch))

;; (define (branch-structure branch)
;;   (cdr branch))
