(define (my-memq item seq)
  (cond ((null? seq) #f)
        ((eq? item (car seq)) seq)
        (else (my-memq item (cdr seq)))))


;;; x53

(list 'a 'b 'c)
;; => (a b c)

(list (list 'george))
;; => ((george))

(cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; => (y1 y2)

(pair? (car '(a short list)))
;; => #f

(memq 'red '((red shoes) (blue socks)))
;; => #f

(memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)


;;; x54

(define (my-equal? seq1 seq2)
  (cond ((and (not (pair? seq1)) (not (pair? seq2))) (eq? seq1 seq2))
        ((and (pair? seq1) (pair? seq2)) (if (eq? (car seq1) (car seq2))
                                             (my-equal? (cdr seq1)
                                                        (cdr seq2))
                                             #f))
        (else #f)))


;;; x55

;; Substitution:
;; (car ''abracadabra)
;; (car '(quote abracadabra))
;; quote
