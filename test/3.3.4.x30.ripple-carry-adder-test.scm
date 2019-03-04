;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
  (string-append (dirname (dirname (current-filename)))
                 "/src/3.3/3.3.4.x28.A_Simulator_for_Digital_Circuits"))

(use-modules (representing-wires))
(use-modules (primitive-function-boxes))
(use-modules (adders))
(use-modules (my-agenda))

(define (make-n-wires n)
  (define (iter n wires)
    (if (<= n 0)
        wires
        (iter (1- n) (cons (make-wire) wires))))
  (iter n '()))

(define (get-signals wires) (map get-signal wires))

(define (set-signals! wires values)
  (if (null? wires)
      'ok
      (begin (set-signal! (car wires) (car values))
             (set-signals! (cdr wires) (cdr values)))))

(set! inverter-delay 2)
(set! and-gate-delay 3)
(set! or-gate-delay 5)

(define half-adder-delay and-gate-delay)

;; carry-delay matters
(define full-adder-delay
  (+ half-adder-delay or-gate-delay))

;; n >= 2
(define (ripple-carry-adder-delay n) (* n full-adder-delay))

;;; Begin test suite
(test-begin "3.3.4.x30.ripple-carry-adder-test")

(define a-wires (make-n-wires 4))
(define b-wires (make-n-wires 4))
(define s-wires (make-n-wires 4))
(define c-out (make-wire))

(ripple-carry-adder a-wires b-wires s-wires c-out)

(set-signals! a-wires '(0 0 0 1))
(set-signals! b-wires '(1 1 1 1))

(test-equal "first number"
  '(0 0 0 1)
  (get-signals a-wires))

(test-equal "second number"
  '(1 1 1 1)
  (get-signals b-wires))

(propagate)

(test-equal "two numbers sum"
  '(0 0 0 0)
  (get-signals s-wires))

(test-equal "carry-out"
  1
  (get-signal c-out))

(test-eq "ripple-carry-adder-delay"
  (ripple-carry-adder-delay (length a-wires))
  (current-time the-agenda))

(test-end "3.3.4.x30.ripple-carry-adder-test")
;;; End test suite
