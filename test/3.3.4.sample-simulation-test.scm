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
(use-modules (probe))

(set! inverter-delay 2)
(set! and-gate-delay 3)
(set! or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(test-begin "3.3.4.sample-simulation-test")

(probe 'sum sum)
(test-equal "sum init"
  '(sum 0 0)
  (get-last-signal-record))

(probe 'carry carry)
(test-equal "carry init"
  '(carry 0 0)
  (get-last-signal-record))

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)
(test-equal "sum after setting input-1 to 1"
  '(sum 8 1)
  (get-last-signal-record))

(set-signal! input-2 1)

(propagate)

(test-equal "sum after setting input-2 to 1"
  '(sum 16 0)
  (get-last-signal-record))

(pop-last-signal-record!)
(test-equal "carry after setting input-2 to 1"
  '(carry 11 1)
  (get-last-signal-record))

(test-end "3.3.4.sample-simulation-test")
