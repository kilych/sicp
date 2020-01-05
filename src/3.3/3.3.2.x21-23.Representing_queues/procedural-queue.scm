;;; SICP implementation
(define-module (procedural-queue)
  #:export (make-queue
            front-ptr
            empty-queue?
            insert-queue!
            delete-queue!
            ;; 3.x21
            print-queue))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "In procedure front-queue: queue is empty")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  ;; cond in sicp
  (if (empty-queue? queue)
      (error "In procedure delete-queue!: queue is empty")
      (set-front-ptr! queue (cdr (front-ptr queue))))
  queue)

;;; 3.x21
;; The standard Scheme printer prints queue-object as is defined, i.e. as
;; cons. The first element is whole list that contains items of
;; the queue. The second is the last pair of the list. Thus, the
;; printer prints the last pair two times.
;; If the first item is the final item in the queue, the front pointer
;; will be the empty list after the deletion, but the rear pointer
;; doesn't, cause we don't worry about updating the rear pointer,
;; which will still point to the pair that contains deleted item, and
;; the printer will print this pair.

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))
