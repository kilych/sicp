(add-to-load-path (dirname (current-filename)))

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/3.3.2.x21-23.Representing_queues"))

(use-modules (representing-wires))
(use-modules (primitive-function-boxes))
(use-modules (adders))
(use-modules (my-agenda))
(use-modules (probe))

;; In contrast to SICP I put definition of the-agenda in the same
;; place as after-delay (my-agenda module).

;; define doesn't mutate variables exported from module
(set! inverter-delay 2)
(set! and-gate-delay 3)
(set! or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'input-1 input-1)
(probe 'input-2 input-2)
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)
