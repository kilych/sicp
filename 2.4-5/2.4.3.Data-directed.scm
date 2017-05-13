;; 'rect and 'polar in the header of the table are for the same column
;; for selectors and constructors

;;; Generic procedures
(define (add z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; Generic selectors and constructors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get-proc proc-table 'make-from-real-imag 'rect) x y))

(define (make-from-mag-ang r a)
  ((get-proc proc-table 'make-from-mag-ang 'polar) r a))

;;; Tagging stuff
(define (attach-tag tag contents) (cons tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

;;; Package installers
(define (install-rectangular-package proc-table)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (let ((x (real-part z))
          (y (imag-part z)))
      (sqrt (+ (* x x) (* y y)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (attach-tag 'rect (cons x y)))
  (define (make-from-mag-ang r a)
    (attach-tag 'rect (cons (* r (cos a)) (* r (sin a)))))
  (add-col proc-table (list 'rect
                            real-part
                            imag-part
                            magnitude
                            angle
                            make-from-real-imag
                            make-from-mag-ang)))

(define (install-polar-package proc-table)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (attach-tag 'polar (cons (sqrt (+ (* x x) (* y y)))
                             (atan y x))))
  (define (make-from-mag-ang r a)
    (attach-tag 'polar (cons r a)))
  (add-col proc-table (list 'polar
                            real-part
                            imag-part
                            magnitude
                            angle
                            make-from-real-imag
                            make-from-mag-ang)))

(define (install-packs proc-table pack-installers)
  (if (null? pack-installers)
      proc-table
      (install-packs ((car pack-installers) proc-table)
                     (cdr pack-installers))))

;;; Table stuff
;; Table is a list of rows. Row is a list of cells.
(define (add-col table col)
  (if (null? table)
      (map list col)
      (map (lambda (cell row) (cons (car row) (cons cell (cdr row))))
           col
           table)))

(define (print-table table)
  (for-each (lambda (row) (display row) (newline))
            table))

(define proc-table (install-packs (add-col '() '(op
                                                 real-part
                                                 imag-part
                                                 magnitude
                                                 angle
                                                 make-from-real-imag
                                                 make-from-mag-ang))
                                  (list install-polar-package
                                        install-rectangular-package)))

(define (get-proc proc-table op type)
  (let ((top-row (car proc-table)))
    (let ((col (- (length top-row) (length (memq type top-row)))))
      (define (find-row op table)
        (cond ((null? table) #f)
              ((eq? op (caar table)) (car table))
              (else (find-row op (cdr table)))))
      (let ((row (find-row op (cdr proc-table))))
        (if row
            (list-ref row col)
            #f)))))

(define (apply-generic op . args)
  (define type (type-tag (car args)))
  (define proc (get-proc proc-table op type))
  (if proc
      (apply proc (map contents args))
      (error "Operator for this type not found in proc-table."
             (list op type))))

;;; Testing stuff
(define (print-complex-nums nums)
  (for-each (lambda (z) (display z) (newline))
            nums))

(define complex-nums (map (lambda (pair)
                            (make-from-real-imag (car pair)
                                                 (cadr pair)))
                          '((0 0)
                            (1 0)
                            (1 -1)
                            (-1 1)
                            (0 2)
                            (1 2)
                            (-1 -4)
                            (-3 3.5))))

(define z0 (car complex-nums))
(define z1 (cadr complex-nums))

(define complex-nums2 (map (lambda (z) (sub (add (div (mul z z) z) z) z))
                           (cdr complex-nums)))
(define complex-nums3 (map (lambda (z) (add (div z z1) z0))
                           complex-nums))
(define complex-nums4 (map (lambda (z) (div z z))
                           (cdr complex-nums)))
