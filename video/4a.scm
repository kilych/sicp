;;           Rule
;; Pattern +------> Skeleton
;; +                +
;; |                |
;; | match          | Instantiation
;; |                |
;; v                v
;; Expression +---> Expression
;; Source           Target

(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x1) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (dd (: x1) (: v)) (: x2))))

    ((dd (** (? x) (?c n)) (? v))
     (* (: n)
        (** (: x) (: (- n 1)))
        (dd (: x) (: v))))))

;; Pattern matching
;; foo - matches exactly foo
;; (f a b) - matches a list whose
;;   first element is f
;;   second element is a
;;   third element is b
;; (? x) - matches anything, call it x
;; (?c x) - matches a constant, call it x
;; (?v x) - matches a variable, call it x

;; Skeletons
;; foo - instantiate to foo
;; (f a b) - instatiate to a list of three elements
;;   which are the results of instantiating each element
;; (: x) - instantiate to the value of x as in the matched pattern

(define dsimp
  (simplifier deriv-rules))

(dsim '(dd (+ x y) x))
;; => (+ 1 0)

(define algebra-rules
  '(
    (((? op) (?c e1) (?c e2))
     (: (op e1 e2)))

    (((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))

    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)))

;; 16:53 - arbitrarily complicated rules

;;                                   +--------------------+
;;                                 +--------------------+ |
;;                               +--------------------+ | |
;;                             +--------------------+ | | |
;;                             |        Rule        | | | +
;;                             +---------+----------+ | +
;;                             | Pattern | Skeleton | +
;;                             +---+-----+----+-----+
;;                                 |          |
;;       +-------------------------+    +-----+
;;       |                              |
;;       v                              v
;; +---------+  Dictionary  +-----------+--+
;; |  match  +------------> |  instantiate |
;; +---------+              +-------+------+
;;     ^                            |
;;     |        Expression          |
;;     +----------------------------+

;;                       Pattern
;;                     +--------+
;;                     |
;;                     v
;;              +------+------+
;;  Expression  |             | Dictionary
;; +----------> |   Matcher   +----------->
;;              |             |
;;              +------+------+
;;                     ^
;;                     | Dictionary
;;                     +

;; Matcher
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         ;; Atomic patterns
         (if (and (atom? exp) (eq? pat exp)) ; nested if instead of
                                             ; and is in original video
             dict
             'failed))
        ;; Pattern variable clauses
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))
        ((atom? exp) 'failed)
        (else (match (cdr pat)
                     (cdr exp)
                     (match (car pat)
                            (car exp)
                            dict)))))

;; pattern
;; (+ (* (? x) (? y)) (? y))
;; expression
;; (+ (* 3 x) 4)
;; simultaneously tree-walk:
;; 1. match "list" (pair really)
;; 2. match +
;; 3. match "list"
;; 4. match *
;; 5. match 3, call it x (in dictionary)
;; 6. match x, call it y
;; 7. match nil
;; 8. match 4, try to call it y, but in dictionary the value of y is
;; x, return 'failed

;;                        Skeleton
;;                       +--------+
;;                       |
;;                       v
;;              +--------+---------+
;;  Dictionary  |                  | Expression
;; +----------> |   Instantiater   +------------>
;;              |                  |
;;              +------------------+

;; Instantiater
(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply (eval (lookup (car form)
                           user-initial-environment)) ; global environment
             (map (lambda (v) (lookup v dict)) ; mapcar in original video
                  (cdr form)))))

;; GIGO - garbage in garbage out

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  ;; using map instead of simplify-parts:
  ;; (define (simplify-exp exp)
  ;;   (try-rules (if (compound? exp)
  ;;                  (map simplify-exp exp)
  ;;                  exp)))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules))
                             exp
                             (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                  (instantiate (skeleton (car rules)) dict))))))
    (scan the-rules))
  simplify-exp)

(define (empty-dictionary) '())

(define (extend-dict pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)                  ; false? in guile ???
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))
