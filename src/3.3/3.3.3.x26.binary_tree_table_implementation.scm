;;; Power of abstraction
;; Only changing of assoc and make-table is needed.
;; (Not only really. Some changes of procedural-insert! are needed too.)

;; Tower of abstractions (abstraction layers)
;;
;; record: pair
;; ---------
;; records: list or binary tree (or something else)
;; ---------
;; table: record when value is records (list or tree)
;; table can contain subtables as records
;; assoc and make-subtable: based on list or tree
;; ---------
;; procedural-lookup and procedural-insert! (with some changes for tree)
;; ---------
;; make-table: message-passing style
;; ---------
;; lookup and insert!

;; OPTIONAL:
;; procedural-insert! implementation independent of records implementation

;; for completeness
(define (make-record key value) (cons key value))
(define (get-key record) (car record))
(define (get-value record) (cdr record))
(define record? pair?)
(define (set-value! record value) (set-cdr! record value))

;; ========== abstraction barrier ========== ;;

;;; unordered list

(define (make-records) '())
(define (add-record record records) (cons record records)) ; add record to list

;; ========== abstraction barrier ========== ;;

;; (define (assoc key records)
;;   (if (not (pair? records))
;;       #f
;;       (let ((record (car records)))
;;         (cond ((not (pair? record)) #f)
;;               ((equal? key (car record)) record)
;;               (else (assoc key (cdr records)))))))

;; (define (make-subtable keys value)
;;   (define (iter reversed-keys subtable)
;;     (if (null?  reversed-keys)
;;         subtable
;;         (iter (cdr reversed-keys) (list (car reversed-keys) subtable))))
;;   (let ((reversed-keys (reverse keys)))
;;     (let ((record (cons (car reversed-keys) value)))
;;       (iter (cdr reversed-keys) record))))

;;; binary tree

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree? tree)
  (and (pair? tree)
       (pair? (cdr tree))
       (pair? (cddr tree))
       (null? (cdddr tree))))

(define (add-record-to-tree! record tree)
  (let ((key (get-key (entry tree))))
    (cond ((eq? (car record) key) (set-car! tree record))
          ((symbol<? (car record) key)
           (let ((branch (left-branch tree)))
             (if (null? branch)
                 (set-car! (cdr tree)
                           (make-tree record '() '()))
                 (add-record-to-tree! record branch))))
          ((symbol>? (car record) key)
           (let ((branch (right-branch tree)))
             (if (null? branch)
                 (set-car! (cddr tree)
                           (make-tree record '() '()))
                 (add-record-to-tree! record branch)))))))

;; ========== abstraction barrier ========== ;;

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))

(define (symbol>? s1 s2)
  (string>? (symbol->string s1)
            (symbol->string s2)))

;; ========== abstraction barrier ========== ;;

;; only strings as keys for simplicity of implementation
(define (assoc key tree)
    (if (not (tree? tree))
        #f
        (let ((record (entry tree)))
          (cond ((not (record? record)) #f)
                ((eq? key (car record)) record)
                ((symbol<? key (car record)) (assoc key (left-branch tree)))
                ((symbol>? key (car record)) (assoc key (right-branch tree)))))))

(define (make-subtable keys value)
  (define (iter reversed-keys subtable)
    (if (null?  reversed-keys)
        subtable
        (iter (cdr reversed-keys)
              ;; (cons (car reversed-keys)
              ;;       (make-tree subtable '() '()))))) ; make records
              (make-tree (cons (car reversed-keys)
                               (make-tree subtable '() '()))
                         '()
                         '()))))        ; make records
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
            ;; Cause I don't know how to implement add-record-to-tree
            ;; in non-recursive (iterative) and non-destructive way.
            ;; (set-cdr! table
            ;;           (add-record-to-tree (make-subtable keys value)
            ;;                               (cdr table)))
            (let ((new-subtable (make-subtable keys value)))
              (if (tree? (cdr table))
                  (add-record-to-tree! new-subtable (cdr table))
                  (set-cdr! table new-subtable))))))
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
