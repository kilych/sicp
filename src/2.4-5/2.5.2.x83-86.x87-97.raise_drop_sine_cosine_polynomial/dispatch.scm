;;; x84. Dispatch
(define (apply-generic op args)
  (define (proc op args) (get op (map type-tag args)))
	(define (result args)
    (let ((res (apply (proc op args) (map contents args))))
      (if (or (eq? op 'raise) (eq? op 'project) (boolean? res))
					res
					(drop res)))) ;until we call constructors without apply-generic
  (let ((len (length args)))
    (cond ((= len 0) (error "APPLY-GENERIC: No args") op)
          ((= len 1)
					 (let ((r (raise-until (lambda (x) (proc op (list x)))
																 (car args))))
						 (if r
								 (result (list r))
								 (let ((ra (raise-until (lambda (x) (proc op (list x)))
																				(drop (car args)))))
									 (if ra
											 (result (list ra))
											 (error "APPLY-GENERIC: No method for this type"
															 (cons op args)))))))
          ((= len 2)
					 (let ((a1 (car args))
								 (a2 (cadr args)))
						 (cond ((proc op args) (result args))
									 ((and (same-type? a1 a2) (proc 'raise (list a1)))
										(apply-generic op (map raise args)))
									 ((not (same-type? a1 a2))
										(let ((r1 (raise-until (lambda (x) (same-type? x a2))
																					 a1))
													(r2 (raise-until (lambda (x) (same-type? a1 x))
																					 a2)))
											(cond (r1 (apply-generic op (list r1 a2)))
														(r2 (apply-generic op (list a1 r2)))
														(else
														 (error "APPLY-GENERIC: Fail to raise args"
																		(cons op args))))))
									 (else (error "APPLY-GENERIC: No method for this types"
																(cons op args))))))
          (else (apply-generic op
                               (cons (apply-generic op
                                                    (list (car args)
                                                          (cadr args)))
                                     (cddr args)))))))
