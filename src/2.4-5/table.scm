;;; Table stuff
(define (make-table) (make-hash-table))

(define (make-key name)
  (define (symbol-list? lst)
    (if (and (pair? lst) (symbol? (car lst)))
        (if (null? (cdr lst)) #t (symbol-list? (cdr lst)))
        #f))
  (cond ((symbol? name) name)
        ((symbol-list? name) (apply symbol-append name))
        (else (error "MAKE-KEY: Invalid name" name))))

(define (put-to table row-name col-name value)
  (define row-key (make-key row-name))
  (define col-key (make-key col-name))
  (define col (hashq-ref table col-key))
  (define new-col (if col col (make-table)))
  (hashq-set! new-col row-key value)
  (hashq-set! table col-key new-col)
  table)

(define (get-from table row-name col-name)
  (let ((row-key (make-key row-name))
        (col-key (make-key col-name)))
    (let ((col (hashq-ref table col-key)))
      (if col
          (hashq-ref col row-key)
          #f))))

(define (get-column table col-name)
  (let ((col-key (make-key col-name)))
    (let ((col (hashq-ref table col-key)))
      (if col
          (hash-fold (lambda (key value seed) (cons value seed)) '() col)
          #f))))
