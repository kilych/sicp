;;; My implementation after quick look at SICP implementation
;; message passing style in contrast to procedural style in SICP

(define-module (my-agenda)
  #:export (make-agenda
            empty-agenda?
            first-agenda-item
            remove-first-agenda-item!
            add-to-agenda!
            the-agenda
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
(define (current-time agenda) (agenda 'current-time))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
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

;;; from 3.x22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "In procedure front-queue: queue is empty")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr (cdr rear-ptr))))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "In procedure delete-queue!: queue is empty")
          (set! front-ptr (cdr front-ptr))))
    (define (print-queue)
      (display front-ptr)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            ((eq? m 'get-items) front-ptr)
            (else (error "In procedure dispatch: undefined operation:" m))))
    dispatch))

(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) (queue 'delete-queue!))
(define (print-queue queue) (queue 'print-queue))
(define (get-items-in-queue queue) (queue 'get-items))
