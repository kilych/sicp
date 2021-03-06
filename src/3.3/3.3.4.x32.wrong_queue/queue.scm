;;; 3.x32
;; "wrong queue": LIFO instead of FIFO

(define-module (queue)
  #:export (make-queue
            queue-items
            front-queue
            empty-queue?
            insert-queue!
            delete-queue!
            print-queue))

(define (make-queue)
  (let ((front-ptr '()))
    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "In procedure front-queue: queue is empty")
          (car front-ptr)))

    (define (insert-queue! item)
      (set! front-ptr (cons item front-ptr)))

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
            ((eq? m 'items) front-ptr)
            (else (error "In procedure dispatch: undefined operation:" m))))

    dispatch))

(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) (queue 'delete-queue!))
(define (print-queue queue) (queue 'print-queue))
(define (queue-items queue) (queue 'items))
