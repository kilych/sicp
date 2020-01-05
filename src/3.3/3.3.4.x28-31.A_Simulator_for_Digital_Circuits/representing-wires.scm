(define-module (representing-wires)
  #:export (make-wire
            get-signal
            set-signal!
            add-action!
            ;; 3.x31
            add-action-wrong!))

;; less verbose naming than in SICP
(define (make-wire)
  (let ((signal 0)
        (actions '()))
    (define (set-signal! new-value)
      (if (not (= signal new-value))
          (begin (set! signal new-value)
                 (call-each actions))
          'done))
    (define (add-action! action)
      (set! actions (cons action actions))
      (action))
    ;; 3.x31
    (define (add-action-wrong! action)
      (set! actions (cons action actions)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            ((eq? m 'add-action-wrong!) add-action-wrong!)
            (else (error "In procedure dispatch: Unknown operation:" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action) ((wire 'add-action!) action))

;; 3.x31
(define (add-action-wrong! wire action) ((wire 'add-action-wrong!) action))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))
