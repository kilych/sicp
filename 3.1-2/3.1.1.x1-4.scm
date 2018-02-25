(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

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

(define acc (make-account 100))


;;; x3.1
;; a-la "make-deposit"
(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

(define A (make-accumulator 5))


;;; x3.2
(define (make-monitored f)
  (let ((count 0))
    (define (monitored-f message)
      (cond ((eq? message 'how-many-calls?) count)
            ((eq? message 'reset-count) (set! count 0))
            (else (set! count (1+ count))
                  (f message))))
    monitored-f))

(define s (make-monitored sqrt))


;;; x3.3
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

(define protected-acc (make-protected-account 100 'asdf))


;;; x3.4
(define (make-cops-protected-account balance password)
  (let ((count 0)
        (max-count 2))                  ;7 in SICP
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops amount)
      (display "You inserted wrong password too many
times.\nWoop-woop!\nThat's the sound of da police!\nWoop-woop!\nThat's
the sound of the beast!\n"))
    (define (dispatch pswd message)
      (cond ((and (not (eq? pswd password)) (= count max-count))
             call-the-cops)
            ((not (eq? pswd password))
             (set! count (1+ count))
             (lambda (amount) "Incorrect password"))
            ((eq? message 'withdraw) (set! count 0) withdraw)
            ((eq? message 'deposit) (set! count 0) deposit)
            (else (lambda (amount) "Unknown method"))))
    dispatch))

(define cops-protected-acc (make-cops-protected-account 100 'asdf))
