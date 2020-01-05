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
(use-modules (my-agenda))

;;; Begin test suite
(test-begin "3.3.4.x29.or-gate-compound-test")

(define in-1 (make-wire))
(define in-2 (make-wire))
(define out (make-wire))

(define or-gate-compound-delay
  (+ (* 2 inverter-delay) and-gate-delay))

(or-gate-compound in-1 in-2 out)

(set-signal! in-1 0)
(set-signal! in-2 1)
(propagate)
(test-eq "0 1 -> 1" 1 (get-signal out))

(test-eq "or-gate-compound-delay"
  or-gate-compound-delay
  (current-time))

(set-signal! in-1 1)
(set-signal! in-2 0)
(propagate)
(test-eq "1 0 -> 1" 1 (get-signal out))

(set-signal! in-1 1)
(set-signal! in-2 1)
(propagate)
(test-eq "1 1 -> 1" 1 (get-signal out))

(set-signal! in-1 0)
(set-signal! in-2 0)
(propagate)
(test-eq "0 0 -> 0" 0 (get-signal out))


(test-end "3.3.4.x29.or-gate-compound-test")
;;; End test suite
