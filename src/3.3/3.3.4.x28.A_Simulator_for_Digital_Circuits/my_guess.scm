;;; My guess implementation
;; (before reading any code of SICP 3.3.4)
;; I thought that the delay is real time delay, but according to SICP
;; we also simulate time.

(define (make-wire)
  (let ((signal 0)
        (out '()))
    (define (set-signal! new-signal)
      (cond ((and (not (zero? new-signal))
                  (not (= 1 new-signal)))
             (error "In procedure set-signal!: value should be 0 or 1"
                    new-signal))
            (else (set! signal new-signal)
                  (if (not (null? out)) (out signal)))))
    (define (set-out! proc) (set! out proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'set-out!) set-out!)
            (else (error "In procedure dispatch: undefined operation:" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-signal) ((wire 'set-signal!) new-signal))
(define (set-out! wire proc) ((wire 'set-out!) proc))

(define (inverter in-wire out-wire)
  (define (invert signal)
    (remainder (+ 1 signal) 2))
  (let ((delay 2))
    (set-out! in-wire
              (lambda (signal)
                (sleep delay)
                (set-signal! out-wire (invert signal))))))

(define (and-gate in-wire-1 in-wire-2 out-wire)
  (define delay 2)
  (define (and-signal s1 s2) (* s1 s2))
  (define (set-out-proc! wire other-wire)
    (set-out! wire
              (lambda (signal)
                (sleep delay)
                (let ((other-signal (get-signal other-wire)))
                  (set-signal! out-wire (and-signal signal other-signal))))))
  (set-out-proc! in-wire-1 in-wire-2)
  (set-out-proc! in-wire-2 in-wire-1))

(define (or-gate in-wire-1 in-wire-2 out-wire)
  (define delay 2)
  (define (or-signal s1 s2) (- (+ s1 s2) (* s1 s2)))
  (define (set-out-proc! wire other-wire)
    (set-out! wire
              (lambda (signal)
                (sleep delay)
                (let ((other-signal (get-signal other-wire)))
                  (set-signal! out-wire (or-signal signal other-signal))))))
  (set-out-proc! in-wire-1 in-wire-2)
  (set-out-proc! in-wire-2 in-wire-1))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(and-gate a b c)

(display (get-signal c))

(newline)

(set-signal! a 1)
(display (get-signal c))

(newline)

(set-signal! b 1)
(display (get-signal c))

(newline)
