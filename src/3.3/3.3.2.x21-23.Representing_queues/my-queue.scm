;;; My implementation just after reading specification

(define-module (my-queue)
  #:export (make-queue
            empty-queue?
            front-queue
            insert-queue!
            delete-queue!))

(define (make-queue)
  (let ((queue '())
        (rear (list '())))
    (define (empty-queue?) (null? queue))

    (define (front-queue)
      (if (empty-queue?)
          (error "In procedure front-queue: queue is empty")
          (car queue)))

    (define (insert-queue! item)
      (set-cdr! rear (list item))
      (set! rear (cdr rear))
      (if (empty-queue?) (set! queue rear))
      queue)

    (define (delete-queue!)
      (if (empty-queue?)
          (error "In procedure delete-queue!: queue is empty")
          (set! queue (cdr queue)))
      queue)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "In procedure dispatch: undefined operation:" m))))

    dispatch))

(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) (queue 'delete-queue!))

(define q (make-queue))
(insert-queue! q 'a)
;; queue contains (a)
(insert-queue! q 'b)
;; queue contains (a b)
(delete-queue! q)
;; queue contains (b)
(insert-queue! q 'c)
;; queue contains (b c)
(insert-queue! q 'd)
;; queue contains (b c d)
(delete-queue! q)
;; queue contains (c d)
