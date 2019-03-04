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

(define probe-history '())

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (set! probe-history
                   (cons (list name
                               (current-time the-agenda)
                               (get-signal wire))
                         probe-history)))))

(set! inverter-delay 2)
(set! and-gate-delay 3)
(set! or-gate-delay 5)

;;; Begin test suite
(test-begin "3.3.4.sample-simulation-test")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(test-equal "sum init" '(sum 0 0) (car probe-history))

(probe 'carry carry)
(test-equal "carry init" '(carry 0 0) (car probe-history))

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)
(test-equal "sum after setting input-1 to 1"
  '(sum 8 1)
  (car probe-history))

(set-signal! input-2 1)
(propagate)
(test-equal "carry after setting input-2 to 1"
  '(carry 11 1)
  (cadr probe-history))
(test-equal "sum after setting input-2 to 1"
  '(sum 16 0)
  (car probe-history))

(test-end "3.3.4.sample-simulation-test")
;;; End test suite
