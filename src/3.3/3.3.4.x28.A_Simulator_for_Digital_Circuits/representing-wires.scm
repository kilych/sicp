(define-module (representing-wires)
  #:export (make-wire
            get-signal
            set-signal!
            add-action!))

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
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else (error "In procedure dispatch: Unknown operation:" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action) ((wire 'add-action!) action))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))