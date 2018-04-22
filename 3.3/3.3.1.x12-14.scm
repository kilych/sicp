;; (define (cons x y)
;;   (let ((new (get-new-pair)))
;;     (set-car! new x)
;;     (set-cdr! new y)
;;     new))

;; get-new-pair is primitive of lisp memory management system
;; see 5.3.1

;;; x12
(define (sicp-append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (my-append! x y)
  (cond ((null? x) (set! x y))
        ((null? (cdr x)) (set-cdr! x y))
        (else (my-append! (cdr x) y)))
  x)

;; SICP
;; Is empty x an error?
(define (sicp-append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; more guarded
(define (my-sicp-append! x y)
  (if (null? x)
      (set! x y)
      (set-cdr! (last-pair x) y))
  x)

;; in guile (last-pair '()) returns ()
;; but (pair? '()) returns #f

(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (sicp-append x y))

z
;; => (a b c d)

;; 1)
(cdr x)
;; => (b)

(define w (sicp-append! x y))

w
;; => (a b c d)

;; 2)
(cdr w)
;; => (b c d)

(cdr x)
;; => (b c d)

;; 1)
;;    +-+-+  +-+-+
;; x->| | |->| |/|
;;    +-+-+  +-+-+
;;     |      |
;;     v      v
;;     a      b
;;
;;    +-+-+  +-+-+
;; z->| | |->| |/|-+
;;    +-+-+  +-+-+ |
;;     |      |    |
;;     v      v    |
;;     a      b    |
;;                 |
;;     +-----------+
;;     |
;;     v
;;    +-+-+  +-+-+
;; y->| | |->| |/|
;;    +-+-+  +-+-+
;;     |      |
;;     v      v
;;     c      d

;; 2)
;;     w
;;     |
;;     v
;;    +-+-+  +-+-+
;; x->| | |->| |/|-+
;;    +-+-+  +-+-+ |
;;     |      |    |
;;     v      v    |
;;     a      b    |
;;                 |
;;     +-----------+
;;     |
;;     v
;;    +-+-+  +-+-+
;; y->| | |->| |/|
;;    +-+-+  +-+-+
;;     |      |
;;     v      v
;;     c      d


;;; x13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define s (make-cycle (list 'a 'b 'c)))

;; (last-pair s)
;; infinite loop cause last-pair uses tail recursion

;;     +------------------+
;;     |                  |
;;     v                  |
;;    +-+-+  +-+-+  +-+-+ |
;; c->| | |->| | |->| |/|-+
;;    +-+-+  +-+-+  +-+-+
;;     |      |      |
;;     v      v      v
;;     a      b      c


;;; x14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define u (mystery v))

u
;; => ???
;; => (d c b a)

v
;; => ???
;; => (a)

;; proc mystery is destructive reverse

;;    +-+-+  +-+-+  +-+-+  +-+-+
;; v->| | |->| | |->| | |->| |/|
;;    +-+-+  +-+-+  +-+-+  +-+-+
;;     |      |      |      |
;;     v      v      v      v
;;     a      b      c      d
;;
;;    +-+-+  +-+-+  +-+-+  +-+-+
;; u->| | |->| | |->| | |->| |/|
;;    +-+-+  +-+-+  +-+-+  +-+-+
;;     |      |      |      |
;;     v      v      v      v
;;     d      c      b      a
;;
;;    +-+-+
;; v->| |/|
;;    +-+-+
;;     |
;;     v
;;     a
