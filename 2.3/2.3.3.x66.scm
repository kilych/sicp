;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) #f)
;;         ((equal? given-key (key (car set-of-records)))
;;          (car set-of-records))
;;         (else (lookup given-key (cdr set-of-records)))))


;;; x66

;; (define (lookup given-key tree-of-records)
;;   (cond ((null? tree-of-records) #f)
;;         ((< given-key (key (entry tree-of-records)))
;;          (lookup given-key (left-branch tree-of-records)))
;;         ((> given-key (key (entry tree-of-records)))
;;          (lookup given-key (right-branch tree-of-records)))
;;         (else (entry tree-of-records))))
