(define-module (probe)
  #:use-module (representing-wires)
  #:use-module (my-agenda)
  #:export (probe
            get-last-signal-record
            pop-last-signal-record!))

(define signal-records '())

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (set! signal-records
                   (cons (list name
                               (current-time)
                               (get-signal wire))
                         signal-records))
                 (display name)
                 (display " ")
                 (display (current-time))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (get-last-signal-record)
  (if (null? signal-records)
      (error "In procedure get-last-signal-record: no records")
      (car signal-records)))

(define (pop-last-signal-record!)
  (let ((last-record (get-last-signal-record)))
    (set! signal-records (cdr signal-records))
    last-record))
