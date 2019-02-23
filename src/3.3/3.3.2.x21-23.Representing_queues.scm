;;; My implementation just after reading specification
(define (my-make-queue)
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

(define (my-empty-queue? queue) (queue 'empty-queue?))
(define (my-front-queue queue) (queue 'front-queue))
(define (my-insert-queue! queue item) ((queue 'insert-queue!) item))
(define (my-delete-queue! queue) (queue 'delete-queue!))

(define q (my-make-queue))
(my-insert-queue! q 'a)
;; queue contains (a)
(my-insert-queue! q 'b)
;; queue contains (a b)
(my-delete-queue! q)
;; queue contains (b)
(my-insert-queue! q 'c)
;; queue contains (b c)
(my-insert-queue! q 'd)
;; queue contains (b c d)
(my-delete-queue! q)
;; queue contains (c d)


;;; SICP implementation
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

(define q (make-queue))
(insert-queue! q 'a)
(front-ptr q)
;; => (a)

(insert-queue! q 'b)
(front-ptr q)
;; => (a b)

(delete-queue! q)
(front-ptr q)
;; => (b)

(insert-queue! q 'c)
(front-ptr q)
;; => (b c)

(insert-queue! q 'd)
(front-ptr q)
;; => (b c d)

(delete-queue! q)
(front-ptr q)
;; => (c d)


;;; x21
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


;;; x22
;; "mp" is for "message passing style"
(define (make-queue-mp)
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

(define (empty-queue-mp? queue) (queue 'empty-queue?))
(define (front-queue-mp queue) (queue 'front-queue))
(define (insert-queue-mp! queue item) ((queue 'insert-queue!) item))
(define (delete-queue-mp! queue) (queue 'delete-queue!))
(define (print-queue-mp queue) (queue 'print-queue))
(define (get-items-in-queue queue) (queue 'get-items))

(display "3.x22 test:")
(newline)

(define mq (make-queue-mp))

(insert-queue-mp! mq 'a)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (a)

(insert-queue-mp! mq 'b)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (a b)

(delete-queue-mp! mq)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (b)

(insert-queue-mp! mq 'c)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (b c)

(insert-queue-mp! mq 'd)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (b c d)

(delete-queue-mp! mq)
(print-queue-mp mq)
(get-items-in-queue mq)
;; => (c d)


;;; x23
;; stuff for doubly linked lists
(define (make-node content prev-ptr next-ptr)
  (cons content (cons prev-ptr next-ptr)))

(define (content node) (car node))
(define (neighbors node) (cdr node))
(define (previous node) (car (neighbors node)))
(define (next node) (cdr (neighbors node)))

(define (set-content! node content) (set-car! node content))
(define (set-previous! node prev) (set-car! (neighbors node) prev))
(define (set-next! node next) (set-cdr! (neighbors node) next))

;; NOTE: All tail pointers should be cleaned up on deletion.
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-deque?) (null? front-ptr))

    (define (front-deque)
      (if (empty-deque?)
          (error "In procedure front-deque: deque is empty")
          (content front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
          (error "In procedure rear-deque: deque is empty")
          (content rear-ptr)))

    (define (front-insert-deque! item)
      (let ((new-front (make-node item '() front-ptr)))
        (if (empty-deque?)
            (set! rear-ptr new-front)
            (set-previous! front-ptr new-front))
        (set! front-ptr new-front)))

    (define (rear-insert-deque! item)
      (let ((new-rear (make-node item rear-ptr '())))
        (if (empty-deque?)
            (set! front-ptr new-rear)
            (set-next! rear-ptr new-rear))
        (set! rear-ptr new-rear)))

    (define (front-delete-deque!)
      (if (empty-deque?)
          (error "In procedure front-delete-deque!: deque is empty"))
      (set! front-ptr (next front-ptr))
      (if (empty-deque?)
          (set! rear-ptr '())
          (set-previous! front-ptr '())))

    (define (rear-delete-deque!)
      (if (empty-deque?)
          (error "In procedure rear-delete-deque!: deque is empty"))
      (set! rear-ptr (previous rear-ptr))
      (if (null? rear-ptr)
          (set! front-ptr '())
          (set-next! rear-ptr '())))

    (define (deque-items)
      (define (iter rear-ptr acc)
        (if (null? rear-ptr)
            acc
            (iter (previous rear-ptr) (cons (content rear-ptr) acc))))
      (if (empty-deque?)
          '()                           ; is error more idiomatic?
          (iter rear-ptr '())))

    (define (print-deque)
      (display (deque-items))
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (empty-deque?))
            ((eq? m 'front-deque) (front-deque))
            ((eq? m 'rear-deque) (rear-deque))
            ((eq? m 'deque-items) (deque-items))
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'print-deque) (print-deque))
            (else (error "In procedure dispatch: undefined operation:" m))))
    dispatch))

(define (empty-deque? deque) (deque 'empty-deque?))
(define (front-deque deque) (deque 'front-deque))
(define (rear-deque deque) (deque 'rear-deque))
(define (deque-items deque) (deque 'deque-items))
(define (front-insert-deque! deque item) ((deque 'front-insert-deque!) item))
(define (rear-insert-deque! deque item) ((deque 'rear-insert-deque!) item))
(define (front-delete-deque! deque) (deque 'front-delete-deque!))
(define (rear-delete-deque! deque) (deque 'rear-delete-deque!))
(define (print-deque deque) (deque 'print-deque))

(newline)
(display "3.x23 test:")
(newline)

(define d (make-deque))
(print-deque d)
(deque-items d)
;; => ()

(front-insert-deque! d 'a)
(print-deque d)
(deque-items d)
;; => (a)

(front-delete-deque! d)
(print-deque d)
(deque-items d)
;; => ()

(front-insert-deque! d 'a)
(print-deque d)
(deque-items d)
;; => (a)

(front-insert-deque! d 'b)
(print-deque d)
(deque-items d)
;; => (b a)

(rear-delete-deque! d)
(print-deque d)
(deque-items d)
;; => (b)

(front-delete-deque! d)
(print-deque d)
(deque-items d)
;; => ()

(rear-insert-deque! d 'c)
(print-deque d)
(deque-items d)
;; => (c)

(rear-insert-deque! d 'd)
(print-deque d)
(deque-items d)
;; => (c d)

(front-delete-deque! d)
(print-deque d)
(deque-items d)
;; => (d)
