;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
  (string-append (dirname (dirname (current-filename)))
                 "/src/3.3/3.3.4.x28-31.A_Simulator_for_Digital_Circuits"))

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.2.x21-23.Representing_queues"))

(use-modules (representing-wires))
(use-modules (primitive-function-boxes))
(use-modules (adders))
(use-modules (my-agenda))

(set! inverter-delay 2)
(set! and-gate-delay 3)
(set! or-gate-delay 5)

(define a-1 (make-wire))
(define b-1 (make-wire))
(define s-1 (make-wire))
(define c-1 (make-wire))

(define a-2 (make-wire))
(define b-2 (make-wire))
(define s-2 (make-wire))
(define c-2 (make-wire))

(half-adder a-1 b-1 s-1 c-1)
(wrong-half-adder a-2 b-2 s-2 c-2)

(set-signal! a-1 1)
(set-signal! a-2 1)

(propagate)

;;; Begin test suite
(test-begin "3.3.4.x31.add-action-wrong-test")

(test-eq "sum-1 equals 1"
  1
  (get-signal s-1))

(test-eq "sum-2 equals 0"
  0
  (get-signal s-2))

(test-end "3.3.4.x31.add-action-wrong-test")
;;; End test suite
