;;; x23
(define-module (deque)
  #:export (make-deque
            deque-items
            empty-deque?
            front-insert-deque!
            rear-insert-deque!
            front-delete-deque!
            rear-delete-deque!
            print-deque))

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
