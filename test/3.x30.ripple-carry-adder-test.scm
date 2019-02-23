;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
  (string-append (dirname (dirname (current-filename)))
                 "/src/3.3/3.3.4.x28.A_Simulator_for_Digital_Circuits"))
(use-modules (representing-wires))
(use-modules (adders))

;;; Begin test suite
(test-begin "ripple-carry-adder-test")

(test-eqv "signal of new wire"
  0
  (get-signal (make-wire)))

(test-end "ripple-carry-adder-test")
;;; End test suite
