(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))


;;; x36

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate op initial (map car sequences))
            (accumulate-n op initial (map cdr sequences)))))


;;; x37

(define (dot-product v w)
  (accumulate +
              0
              (accumulate-n *
                            1
                            (list v w))))

(define (dot-product2 v w)
  (accumulate +
              0
              (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v))
       m))

(define (matrix-*-matrix m n)
  (map (lambda (v) (matrix-*-vector (transpose n) v))
       m))

(define (transpose m)
  (accumulate-n cons
                '()
                m))

(define (unity-vector n)
  (if (= n 0)
      '()
      (cons 1 (unity-vector (- n 1)))))

(define m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))

(define p_x '((1 0)
              (0 0)))

(define p_y '((0 0)
              (0 1)))

(define e '((1 0)
            (0 1)))
