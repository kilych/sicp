(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;; less verbose naming than in SICP
(define (and-gate in1 in2 output)
  (define (and-action)
    (let ((new-value
           (logical-and (get-signal in1) (get-signal in2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! in1 and-action)
  (add-action! in2 and-action)
  'ok)


;;; x28
(define (or-gate in1 in2 output)
  (define (or-action)
    (let ((new-value
           (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! in1 or-action)
  (add-action! in2 or-action)
  'ok)


;;; x29
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

;; it was (* s1 s2) instead of (else (* s1 s2)) and in sample
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
(define (logical-and s1 s2)
  (cond ((and (not (= s1 0)) (not (= s1 1)))
         (error "In procedure logical-and: Invalid signal s1" s1))
        ((and (not (= s2 0)) (not (= s2 1)))
         (error "In procedure logical-and: Invalid signal s2" s2))
        (else (* s1 s2))))

(define (logical-or s1 s2)
  (cond ((and (not (= s1 0)) (not (= s1 1)))
         (error "In procedure logical-or: Invalid signal s1" s1))
        ((and (not (= s2 0)) (not (= s2 1)))
         (error "In procedure logical-or: Invalid signal s2" s2))
        (else (- (+ s1 s2) (* s1 s2)))))
