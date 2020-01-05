;;; My implementation after quick look at SICP implementation
;; message passing style in contrast to procedural style in SICP

(define-module (my-agenda)
  #:use-module (queue)
  #:export (;; exporting make-agenda has no sense since after-delay
            ;; and propagate use module-wide agenda object
            ;; make-agenda
            after-delay
            propagate)
  #:replace (current-time))

(define (make-agenda)
  (let ((time-table (list 0)))
    (define (empty-agenda?) (null? (cdr time-table)))

    (define (first-item)
      (if (empty-agenda?)
          (error "In procedure first-item: agenda is empty")
          ;; after more deeper reading of SICP implementation
          ;; maybe we need to set current time on removing item (???)
          (let ((time-segment (cadr time-table)))
            (set-car! time-table (car time-segment))
            (front-queue (cdr time-segment)))))

    (define (remove-first-item!)
      (if (empty-agenda?)
          (error "In procedure remove-first-item!: agenda is empty")
          (let ((queue (cdadr time-table)))
            (delete-queue! queue)
            (if (empty-queue? queue) (set-cdr! time-table (cddr time-table))))))

    (define (add-to-agenda! time proc)
      (define (add! time proc table)
        (cond ((or (null? (cdr table)) (< time (caadr table)))
               (let ((queue (make-queue)))
                 (insert-queue! queue proc)
                 (set-cdr! table (cons (cons time queue) (cdr table)))))
              ((= time (caadr table))
               (let ((queue (cdadr table)))
                 (insert-queue! queue proc)))
              (else (add! time proc (cdr table))))
        'ok)
      (add! time proc time-table))

    (define (dispatch m)
      (cond ((eq? m 'empty-agenda?) (empty-agenda?))
            ((eq? m 'first-agenda-item) (first-item))
            ((eq? m 'remove-first-agenda-item!) (remove-first-item!))
            ((eq? m 'add-to-agenda!) add-to-agenda!)
            ((eq? m 'current-time) (car time-table))
            (else (error "In procedure dispatch: undefined operation:" m))))

    dispatch))

(define (empty-agenda? agenda) (agenda 'empty-agenda?))
(define (first-agenda-item agenda) (agenda 'first-agenda-item))
(define (remove-first-agenda-item! agenda) (agenda 'remove-first-agenda-item!))
(define (add-to-agenda! time item agenda)
  ((agenda 'add-to-agenda!) time item))

(define the-agenda (make-agenda))

(define (current-time) (the-agenda 'current-time))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(define (assoc time records)
  (cond ((null? records) #f)
        ((< time (caar records)) #f)
        ((= time (caar records)) (car records))
        (else (assoc time (cdr records)))))

(define (lookup time table)
  (let ((record (assoc time (cdr table))))
    (if record
        (cdr record)
        #f)))
