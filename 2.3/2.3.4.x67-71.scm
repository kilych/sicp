(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? 'leaf (car obj)))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

;; simple generic procedures: can works with more than one type of data
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;; It is too complicated in SICP.
(define (decode bits tree)
  (define (decode-1 bits branch)
    (cond ((leaf? branch)
           (cons (symbol-leaf branch) (decode-1 bits tree)))
          ((null? bits) '())
          ((= 1 (car bits)) (decode-1 (cdr bits) (right-branch branch)))
          ((= 0 (car bits)) (decode-1 (cdr bits) (left-branch branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;;; x67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-message2 '(0 1 0))

(decode sample-message sample-tree)
;; => (A D A B B C A)


;;; x68

(define (encode symbs tree)
  (define (encode-1 symb branch)
    (cond ((leaf? branch) (encode (cdr symbs) tree))
          ((memq symb (symbols (left-branch branch)))
           (cons 0 (encode-1 symb (left-branch branch))))
          ((memq symb (symbols (right-branch branch)))
           (cons 1 (encode-1 symb (right-branch branch))))
          (else (error "Unexpected symbol:" symb))))
  (if (null? symbs)
      '()
      (encode-1 (car symbs) tree)))

;; SICP-way
(define (sicp-encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (sicp-encode (cdr message) tree))))

(define (encode-symbol symb tree)
  (cond ((leaf? tree) '())
        ((memq symb (symbols (left-branch tree)))
         (cons 0 (encode-symbol symb (left-branch tree))))
        ((memq symb (symbols (right-branch tree)))
         (cons 1 (encode-symbol symb (right-branch tree))))
        (else (error "Unexpected symbol:" symb))))


;;; x69

(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))

(define (succesive-merge trees)
  (cond ((null? trees) '())
        ((null? (cdr trees)) (car trees))
        (else (succesive-merge (adjoin-set (make-code-tree (car trees)
                                                           (cadr trees))
                           (cddr trees))))))


;;; x70

(define rock-tree (generate-huffman-tree '((A 2)
                                           (BOOM 1)
                                           (GET 2)
                                           (JOB 2)
                                           (NA 16)
                                           (SHA 3)
                                           (YIP 9)
                                           (WAH 1))))
(define song '(GET A JOB
               SHA NA NA NA NA NA NA NA NA
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
               SHA BOOM))

;; How many bites?
(length (encode song rock-tree))
;; => 84

;; 36 symbols (words) * 3 bites = 108 bites for fixed-length code.


;;; x71

;; One bit is for the most common symbol.
;; n - 1 bites are for the rarest symbol.

;; Recursive Huffman's tree: one leaf on right and the rest on left,
;; and so on, because 1+2 < 4, 1+2+4 < 8..


;;; x72
