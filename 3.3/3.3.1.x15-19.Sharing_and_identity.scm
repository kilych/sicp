;; eq? compares pointers

(eq? (cons 'a 'b) (cons 'a 'b))
;; => #f

(eq? (list 'a 'b) (list 'a 'b))
;; => #f

;; symbols are shared
(eq? 'a 'a)
;; => #t

(define x (list 'a 'b))

(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

z1
;; => ((a b) a b)

z2
;; => ((a b) a b)

(define (set-to-wow! x)
  (set-car! (car x) 'wow))

(set-to-wow! z1)
(set-to-wow! z2)

z1
;; => ((wow b) wow b)

z2
;; => ((wow b) a b)


;;; x15
;; Before set-to-wow!:
;;
;;     +-+-+
;; z1->| | |--+
;;     +-+-+  |
;;      |     v
;;      +--> +-+-+  +-+-+
;;        x->| | |->| |/|
;;           +-+-+  +-+-+
;;            |      |
;;            v      v
;;            a      b
;;
;;     +-+-+
;; z2->| | |--+
;;     +-+-+  |
;;      |     v
;;      |    +-+-+  +-+-+
;;      |    | | |->| |/|
;;      |    +-+-+  +-+-+
;;      |     |      |
;;      |     v      v
;;      |     a      b
;;      |     ^      ^
;;      |     |      |
;;      |    +-+-+  +-+-+
;;      +--->| | |->| |/|
;;           +-+-+  +-+-+

;; After set-to-wow!:
;;
;;     +-+-+
;; z1->| | |--+
;;     +-+-+  |
;;      |     v
;;      +--> +-+-+  +-+-+
;;        x->| | |->| |/|
;;           +-+-+  +-+-+
;;            |      |
;;            v      v
;;           wow     b
;;
;;     +-+-+
;; z2->| | |--+
;;     +-+-+  |
;;      |     v
;;      |    +-+-+  +-+-+
;;      |    | | |->| |/|
;;      |    +-+-+  +-+-+
;;      |     |      |
;;      |     v      v
;;      |    wow     b
;;      |     ^      ^
;;      |     |      |
;;      |    +-+-+  +-+-+
;;      +--->| | |->| |/|
;;           +-+-+  +-+-+


;;; x16
(define (count-pairs-diddle x)
  (if (not (pair? x))
      0
      (+ (count-pairs-diddle (car x))
         (count-pairs-diddle (cdr x))
         1)))

;; 3
(define s3 (list 'a 'b 'c))

(count-pairs-diddle s3)
;; => 3

;;     +-+-+  +-+-+  +-+-+
;; s3->| | |->| | |->| |/|
;;     +-+-+  +-+-+  +-+-+
;;      |      |      |
;;      v      v      v
;;      a      b      c

;; 4
(define s4 (list 'a 'b 'c))

(set-car! (cdr s4 ) (cddr s4))

(count-pairs-diddle s4)
;; => 4

;;             +------+
;;             |      |
;;             |      v
;;     +-+-+  +-+-+  +-+-+
;; s4->| | |->| | |->| |/|
;;     +-+-+  +-+-+  +-+-+
;;      |             |
;;      v             v
;;      a             c

;; 7
(define s7 (list 'a 'b 'c))

(set-car! (cdr s7) (cddr s7))
(set-car! s7 (cdr s7))

(count-pairs-diddle s7)
;; => 7

;;             +------+
;;             |      |
;;             |      v
;;     +-+-+  +-+-+  +-+-+
;; s7->| | |->| | |->| |/|
;;     +-+-+  +-+-+  +-+-+
;;      |      ^      |
;;      |      |      v
;;      +------+      c

;; infinity
;; from 3.3.1.x13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define s-inf (make-cycle (list 'a 'b 'c)))

;; (count-pairs-diddle s-inf)
;; => stack overflow (the proc produces recursive process)

;;         +------------------+
;;         |                  |
;;         v                  |
;;        +-+-+  +-+-+  +-+-+ |
;; s-inf->| | |->| | |->| |/|-+
;;        +-+-+  +-+-+  +-+-+
;;         |      |      |
;;         v      v      v
;;         a      b      c


;;; x17
(define (count-pairs x)
  (let ((visited '()))
    (define (known? pair)
      (memq pair visited))
    (define (keep pair)
      (set! visited (cons pair visited)))
    (define (walk pair)
      (cond ((not (pair? pair)) 0)
            ((known? pair) 0)
            (else
              (keep pair)
              (+ 1
                 (walk (car pair))
                 (walk (cdr pair))))))
    (walk x)))

;; also we can get length of the visited as result


;;; x18
;; taking only cdrs
(define (cyclic-list? x)
  (define (walk x visited)
    (define (known?) (memq x visited))
    (cond ((not (pair? x)) #f)
          ((known?) #t)
          (else (walk (cdr x) (cons x visited)))))
  (walk x '()))

(cyclic-list? s3)
;; => #f

(cyclic-list? s4)
;; => #f

(cyclic-list? s7)
;; => #f

(cyclic-list? s-inf)
;; => #t

;; OPTIONAL:
;; more general
(define (cyclic-tree? x)
  (define (walk x visited)
    (define (known?) (memq x visited))
    (cond ((not (pair? x)) #f)
          ((known?) #t)
          (else (or (walk (car x) (cons x visited))
                    (walk (cdr x) (cons x visited))))))
  (walk x '()))

(define cyclic-tree (list 'a 'b 'c))
(set-car! (cdr cyclic-tree) cyclic-tree)

(cyclic-tree? s3)
;; => #f

(cyclic-tree? s4)
;; => #f

(cyclic-tree? s7)
;; => #f

(cyclic-tree? s-inf)
;; => #t

(cyclic-list? cyclic-tree)
;; => #f

(cyclic-tree? cyclic-tree)
;; => #t


;;; x19
;; Feature of list with cycle is that our reverse! proc makes reversed
;; only cycle part.
;; Second call of reverse! reverses list (with or without cycle) back
;; to its original structure.
(define (cyclic-list!? lst)
  (define (reverse! reversed rest)
    (if (null? rest)
        reversed
        (let ((temp (cdr rest)))
          (set-cdr! rest reversed)
          (reverse! rest temp))))
  (let ((reversed (reverse! '() lst)))
    (reverse! '() reversed)
    ;; name reversed still points to pair that was first after first mutation
    (eq? lst reversed)))

(define noose (list 'a 'b 'c))
(set-cdr! (last-pair noose) (cdr noose))

(define little-noose (list 'a 'b 'c))
(set-cdr! (last-pair little-noose) (last-pair little-noose))

(cyclic-list!? s3)
;; => #f

(cyclic-list!? s4)
;; => #f

(cyclic-list!? s7)
;; => #f

(cyclic-list!? s-inf)
;; => #t

(cyclic-list!? noose)
;; => #t

(cyclic-list!? little-noose)
;; => #t
