;; Closure property: subtable is list, but record is cons-pair ???

;; This simple variant does not expect atomar value - list of records
;; only.
;; Break it:
;; (define t (make-table))
;; (insert! '(a) 1 t)
;; (insert! '(a b) 2 t)
;;
;; (define (assoc key records)
;;   (cond ((null? records) #f)
;;         ((equal? key (caar records)) (car records))
;;         (else (assoc key (cdr records)))))

(define (assoc key records)
  (if (not (pair? records))
      #f
      (let ((record (car records)))
        (cond ((not (pair? record)) #f)
              ((equal? key (car record)) record)
              (else (assoc key (cdr records)))))))

;; (define t (make-table))
;; (insert! '(a) 1 t)
;; (insert! '(a b c) 2 t)
;; (lookup '(a) t)
;; => ((b (c . 2)) . 1)
;; Is this trailing value (1) problem?

(define (make-subtable keys value)
  (define (iter reversed-keys subtable)
    (if (null?  reversed-keys)
        subtable
        (iter (cdr reversed-keys) (list (car reversed-keys) subtable))))
  (let ((reversed-keys (reverse keys)))
    (let ((record (cons (car reversed-keys) value)))
      (iter (cdr reversed-keys) record))))

;; ========== abstraction barrier ========== ;;

(define (procedural-lookup keys table)
  (if (null? keys)
      (cdr table)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (procedural-lookup (cdr keys) subtable)
            #f))))

(define (procedural-insert! keys value table)
  (if (null? keys)
      (set-cdr! table value)
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
            (procedural-insert! (cdr keys) value subtable)
            (set-cdr! table
                      (cons (make-subtable keys value)
                            (cdr table))))))
  'ok)

;; ========== abstraction barrier ========== ;;

(define (make-table)
  ;; headed list for being similar to any subtable
  ;; (but record: record is cons-pair not list)
  (let ((table (list '*table*)))

    (define (lookup keys)
      (if (null? keys)  ; guard encapsulation: do not return whole table
          (error "In procedure lookup: at least one key should be passed")
          (procedural-lookup keys table)))

    (define (insert! keys value)
      (if (null? keys)
          (error "In procedure insert!: at least one key should be passed")
          (procedural-insert! keys value table)))

    (define (dispatch m)
      (cond
       ;; ((eq? m 'get-local-table) table) ; it breaks encapsulation: debug only
       ((eq? m 'lookup-proc) lookup)
       ((eq? m 'insert!-proc) insert!)
       (error "In procedure dispatch: undefined operation:" m)))

    dispatch))

;; ========== abstraction barrier ========== ;;

(define (lookup keys table) ((table 'lookup-proc) keys))
(define (insert! keys value table) ((table 'insert!-proc) keys value))
