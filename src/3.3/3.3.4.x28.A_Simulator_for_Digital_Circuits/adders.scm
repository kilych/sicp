(define-module (adders)
  #:use-module (representing-wires)
  #:use-module (primitive-function-boxes)
  #:export (half-adder
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

;;; x30
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
