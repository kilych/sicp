;; ;; a.
;; (define (get-record file name-of-employee)
;;   (apply-generic 'get-record file name-of-employee))

;; ;; b.
;; (define (get-salary file name-of-employee)
;;   (let ((record (get-record file name-of-employee)))
;;     ((get-proc proc-table
;;                'get-salary
;;                (type-of-enterprise file)) record)))

;; ;; c.
;; (define (find-employee-record files name-of-employee)
;;   (if (null? files)
;;       #f
;;       (let ((record (get-record (car file) name-of-employee)))
;;         (if record
;;             record
;;             (find-employee-record (cdr files) name-of-employee)))))

;; ;; d.
;; ;; Just adding column to proc-table is for new type of enterprise.

;; ;; Only files have attached type-tag.
;; (define (apply-generic op . args)
;;   (define (file-from-args args)
;;     (cond ((null? args) #f)
;;           ((file? (car args)) (car args))
;;           (else (file-from-args (cdr args)))))
;;   (let ((type (type-of-enterprise (file-from-args args))))
;;     (let ((proc (get-proc proc-table op type)))
;;       (if proc
;;           (apply proc args)
;;           (error "Operator for this type not found in proc-table."
;;                  (list op type))))))
