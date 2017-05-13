;; Other signature for add-col:
;; col,table->table instead table,col->table

;;; Derivative
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        (else ((get-proc proc-table 'deriv (operator exp)) (operands exp)
               var))))

(define (var? x) (symbol? x))
(define (same-var? x y) (and (var? x) (var? y) (eq? x y)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package proc-table)
  (define (deriv-sum args var)
    (if (null? (cdr args))
        (deriv (car args) var)
        (cons '+ (map (lambda (exp) (deriv exp var))
                      args))))
  (define (deriv-product args var)
    (if (null? (cdr args))
        (deriv (car args) var)
        (let ((first (car args))
              (rest (if (null? (cddr args))
                        (cadr args)
                        (cons '* (cdr args)))))
          (cons '+ (list (list '* first (deriv rest var))
                         (list '* (deriv first var) rest))))))
  ;; c:
  (define (deriv-expt args var)
    (let ((base (car args))
          (expt (if (and (not (null? (cdr args))) (real? (cadr args)))
                    (cadr args)
                    (error "Unexpected exponent"))))
      (list '* expt (list '** base (- expt 1)) (deriv base var))))
  (add-col (list '+
                 deriv-sum)
           (add-col (list '*
                          deriv-product)
                    (add-col (list '**
                                   deriv-expt)
                             proc-table))))

;;; Table stuff
;; Table is a list of rows. Row is a list of cells.
(define (add-col col table)
  (if (null? table)
      (map list col)
      (map (lambda (cell row) (cons (car row) (cons cell (cdr row))))
           col
           table)))

(define (print-table table)
  (for-each (lambda (row) (display row) (newline))
            table))

(define (get-proc proc-table op type)
  (let ((top-row (car proc-table)))
    (let ((col (- (length top-row) (length (memq type top-row)))))
      (define (find-row op table)
        (cond ((null? table) #f)
              ((eq? op (caar table)) (car table))
              (else (find-row op (cdr table)))))
      (let ((row (find-row op (cdr proc-table))))
        (if row
            (list-ref row col)
            #f)))))

;;; Making proc-table
(define proc-table (install-deriv-package (add-col '(op
                                                     deriv)
                                                   '())))

;;; Exercise
;; a. Numbers and variables are not expressions.
;; b. All this stuff is for this work.
;; c. Done.

;; d. If we use
;; ((get-proc proc-table (operator exp) 'deriv) (operands exp) var)
;; than
;; 1) In tail of install-deriv-package:
;; (add-col (list 'deriv
;;                deriv-sum
;;                deriv-product
;;                deriv-expt)
;;          proc-table)
;; 2) new init-proc-table
;; (define init-proc-table (add-col '(op
;;                                    +
;;                                    *
;;                                    **)
;;                                  '()))
