(define (pascal-triangle item raw)
  (cond ((or (< item 0)
             (< raw 0)
             (> item raw)) 0)
        ((or (= item 0)
             (= item raw)) 1)
        (else (+ (pascal-triangle (- item 1) (- raw 1))
                 (pascal-triangle item (- raw 1))))))

;; Optional
;; Iterative implementation.
