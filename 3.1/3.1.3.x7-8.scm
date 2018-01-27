(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

;; Substitution:
;; (W 20)
;; ((make-simplified-withdraw 25) 20)
;; ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;; (set! balance (- 25 20)) 25

(define (make-decrementer balance)
  (lambda (amount) (- balance amount)))

;; D1 and D2 are the same because they have the same behavior.
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))

;; W1 and W2 are not the same because behavior of them is not.
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          ;; error in SICP:
          ;; (else (error  "MAKE-ACCOUNT: Unknown method:" message))
          (else (lambda (amount) "Unknown method"))))
  dispatch)

;; two names of the same object
(define peter-acc (make-account 100))
(define paul-acc peter-acc)

(define (factorial n)
  (define (iter n product)
    (if (zero? n)
        product
        (iter (1- n) (* n product))))
  (iter n 1))

;; order of assignments is matter
(define (factorial-imperative n)
  (let ((product 1))
    (define (iter)
      (if (zero? n)
          product
          (begin (set! product (* n product))
                 (set! n (1- n))
                 (iter))))
    (iter)))


;;; x7
;; from x3:
(define (make-protected-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pswd message)
    (if (eq? pswd password)
        (cond ((eq? message 'withdraw) withdraw)
              ((eq? message 'deposit) deposit)
              (else (lambda (amount) "Unknown method")))
        (lambda (amount) "Incorrect password")))
  dispatch)

(define (make-joint acc password new-password)
  (define (adopt pswd message)
    (if (eq? pswd new-password)
        (acc password message)
        (lambda (amount) "Incorrect new joint password")))
  adopt)

(define A1 (make-protected-account 100 'open-sesam))
(define A2 (make-joint A1 'open-sesam 'rosebud))


;;; x8, 2016
(define f
  (let ((old-arg 0))
    (lambda (arg)
      (cond ((and (= old-arg 0) (= arg 0)) (set! old-arg arg) 0)
            ((and (= old-arg 0) (= arg 1)) (set! old-arg arg) 0)
            ((and (= old-arg 1) (= arg 0)) (set! old-arg arg) 1)
            ((and (= old-arg 1) (= arg 1)) (set! old-arg arg) 0)))))

;; FIXME:
;; first call (+ (f 0) (f 1)) returns 0, the next one returns 1
;; (+ (f 1) (f 0)) returns the same every time

;;; x8, 2017, november
(define f
  ;; initial state
  (let ((counter 0)
        (old-arg -1))
    (lambda (arg)
      (define res
        (if (and (= counter 1) (= old-arg 1) (zero? arg))
            1
            0))
      (if (zero? counter) (set! counter 1) (set! counter 0))
      (set! old-arg arg)
      res)))
