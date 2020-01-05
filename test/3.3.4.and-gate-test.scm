(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

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
(use-modules (probe))

(set! and-gate-delay 1)

(test-begin "and-gate")

(define in-1 (make-wire))
(define in-2 (make-wire))
(define out (make-wire))

(probe 'in-1 in-1)
(probe 'in-2 in-2)
(probe 'out out)

(set-signal! in-2 1)

(and-gate in-1 in-2 out)

(propagate)

(test-eq "initial state of in-1" 0 (get-signal in-1))
(test-eq "initial state of in-2" 1 (get-signal in-2))
(test-eq "initial state of out" 0 (get-signal out))

(set-signal! in-1 1)
(set-signal! in-2 0)

(propagate)

(test-eq "inputs change from 0, 1 to 1, 0 in the same segment"
  0
  (get-signal out))

(test-end "and-gate")
