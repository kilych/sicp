(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;; (non-working) hack for fibonacci memoization
;; (define (assoc key records)
;;   (cond ((null? records) #f)
;;         ((> key (caar records)) #f)
;;         ((equal? key (caar records)) (car records))
;;         (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table) (list '*table*))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (arg)
      (let ((previously-computed-result (lookup arg table)))
        (or previously-computed-result
            (let ((result (f arg)))
              (insert! arg result table)
              result))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; It doesn't work right, cause in this case we call recursively not
;; memoized procedure fib.
;; (define memo-fib (memoize fib))

;; It works right.
;; (define fib (memoize fib))

;; We need n deffered operations before we
;; call memo-fib on 0 and 1 (assuming local table is empty at the
;; beginning), after that we start to memoize results from smallest to
;; largest arg, and, cause fibonacci requires results of itself for
;; smaller args, this results always will be already computed.
;; And cause assoc steps for search n is O(n) in this process
;; (arithmetic progression), final number of steps will be n^2/2,
;; i.e. O(n^2).
;; If assoc steps is O(1), then memo-fib steps is O(n).

;; TODO: environment diagram
