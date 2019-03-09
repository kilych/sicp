(define-module (primitive-function-boxes)
  #:use-module (representing-wires)
  #:use-module (my-agenda)
  #:export (inverter
            and-gate
            or-gate
            or-gate-compound
            inverter-delay
            and-gate-delay
            or-gate-delay
            ;; 3.x31
            wrong-inverter
            wrong-and-gate
            wrong-or-gate))

(define inverter-delay 0)
(define and-gate-delay 0)
(define or-gate-delay 0)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;; less verbose naming than in SICP
(define (and-gate in-1 in-2 output)
  (define (and-action)
    (let ((new-value
           (logical-and (get-signal in-1) (get-signal in-2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! in-1 and-action)
  (add-action! in-2 and-action)
  'ok)


;;; 3.x28
(define (or-gate in-1 in-2 output)
  (define (or-action)
    (let ((new-value
           (logical-or (get-signal in-1) (get-signal in-2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! in-1 or-action)
  (add-action! in-2 or-action)
  'ok)


;;; 3.x29
(define (or-gate-compound input-1 input-2 output)
  (let ((inverted-1 (make-wire))
        (inverted-2 (make-wire))
        (mul (make-wire)))
    (inverter input-1 inverted-1)
    (inverter input-2 inverted-2)
    (and-gate inverted-1 inverted-2 mul)
    (inverter mul output)
    'ok))

;; compound or-gate delay = inverter delay + and-gate delay + inverter delay


(define (logical-not s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (else (error "In procedure logical-not: Invalid signal" s))))

;; it was (* s-1 s-2) instead of (else (* s-1 s-2)) and in sample
;; simulation of half-adder
;; at time 0: input-1 = 0 and input-2 = 0
;; at time 2 (after inverter delay): inverted carry (and of inputs) = 1
;; at time 5 (after additional and-delay): sum = 1 and probe called
;; cause logical-and returns 1 when second arg (inverted carry in case
;; of sum) = 1 independently of first arg
;; when set input-1 = 1 at time 0
;; or-wire = 1 at time 5 (after or-delay) but at time 8 (after
;; additional and-delay) sum signal was 1 already and actions of sum
;; (probe) was not called
(define (logical-and s-1 s-2)
  (cond ((and (not (= s-1 0)) (not (= s-1 1)))
         (error "In procedure logical-and: Invalid signal s-1" s-1))
        ((and (not (= s-2 0)) (not (= s-2 1)))
         (error "In procedure logical-and: Invalid signal s-2" s-2))
        (else (* s-1 s-2))))

(define (logical-or s-1 s-2)
  (cond ((and (not (= s-1 0)) (not (= s-1 1)))
         (error "In procedure logical-or: Invalid signal s-1" s-1))
        ((and (not (= s-2 0)) (not (= s-2 1)))
         (error "In procedure logical-or: Invalid signal s-2" s-2))
        (else (- (+ s-1 s-2) (* s-1 s-2)))))


;;; 3.x31
(define (wrong-inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action-wrong! input invert-input)
  'ok)

(define (wrong-and-gate in-1 in-2 output)
  (define (and-action)
    (let ((new-value
           (logical-and (get-signal in-1) (get-signal in-2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action-wrong! in-1 and-action)
  (add-action-wrong! in-2 and-action)
  'ok)

(define (wrong-or-gate in-1 in-2 output)
  (define (or-action)
    (let ((new-value
           (logical-or (get-signal in-1) (get-signal in-2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action-wrong! in-1 or-action)
  (add-action-wrong! in-2 or-action)
  'ok)
