(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((table '()))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 table)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1  table)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set! table
                  (cons (list key-1
                              (cons key-2 value))
                        table))))
      'ok)

    (define (dispatch m)
      (cond
       ;; ((eq? m 'get-local-table) table) ; it breaks encapsulation: debug only
       ((eq? m 'lookup-proc) lookup)
       ((eq? m 'insert!-proc) insert!)
       (error "In procedure dispatch: undefined operation:" m)))

    dispatch))

(define (lookup key-1 key-2 table) ((table 'lookup-proc) key-1 key-2))
(define (insert! key-1 key-2 value table)
  ((table 'insert!-proc) key-1 key-2 value))
