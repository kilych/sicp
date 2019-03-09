(define-module (adders)
  #:use-module (representing-wires)
  #:use-module (primitive-function-boxes)
  #:export (half-adder
            wrong-half-adder            ; 3.x31
            full-adder
            ripple-carry-adder))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in s c-out)
  (let ((c-1 (make-wire)) (c-2 (make-wire)) (s-1 (make-wire)))
    (half-adder a b s-1 c-1)
    (half-adder s-1 c-in s c-2)
    (or-gate c-1 c-2 c-out)
    'ok))

;;; 3.x30
(define (ripple-carry-adder a-wires b-wires s-wires c-out)
  (if (null? a-wires)
      'ok
      (let ((c-in (make-wire)))
        (full-adder (car a-wires)
                    (car b-wires)
                    c-in
                    (car s-wires)
                    c-out)
        (ripple-carry-adder (cdr a-wires)
                            (cdr b-wires)
                            (cdr s-wires)
                            c-in))))

;; 3.x31
;; When wrong-half-adder is executed on wires with zero initial signal,
;; internal inverter didn't invert signal from c in e, because set-signal!
;; action isn't executed when inverter connects c and e.
;; For example, after setting signal of a-input to 1, signal of sum
;; should be 1, but signal of e is 0 instead of 1, then signal of sum
;; remains 0.
(define (wrong-half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (wrong-or-gate a b d)
    (wrong-and-gate a b c)
    (wrong-inverter c e)
    (wrong-and-gate d e s)
    'ok))
