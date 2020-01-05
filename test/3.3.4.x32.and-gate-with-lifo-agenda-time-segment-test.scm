(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.4.x28-31.A_Simulator_for_Digital_Circuits"))

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.4.x32.wrong_queue"))

(use-modules (representing-wires))
(use-modules (primitive-function-boxes))
(use-modules (my-agenda))
(use-modules (probe))

(set! and-gate-delay 1)

(test-begin "and-gate with LIFO agenda time segment")

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
  1
  (get-signal out))

(test-end "and-gate with LIFO agenda time segment")

;;; 3.x32
;; Assume and-gate-delay equals 1.
;; When inputs change from 0, 1 to 1, 0 in the same segment
;; corresponding to the moment of (simulated) time 1, at first,
;; we puts '(set-signal! out 1) into segment of moment 2,
;; then we puts '(set-signal! out 0) into the same segment.
;; When current time is 2, at first, we pop and call '(set-signal! out 0),
;; because LIFO. Nothing happens: out signal is already 0.
;; Then we pop and call '(set-signal! out 1):
;; out signal is changed to wrong value 1.
;; See 3.3.4.and-gate-test.scm to trace proper behavior.
