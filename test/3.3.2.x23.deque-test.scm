;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.2.x21-23.Representing_queues"))

(use-modules (deque))

(test-begin "3.3.2.x23: message passing style implementation of deque")

(define d (make-deque))

(test-assert "new deque is empty"
  (empty-deque? d))

(print-deque d)

(front-insert-deque! d 'a)
(test-equal "one element in deque"
  '(a)
  (deque-items d))

(print-deque d)

(front-delete-deque! d)
(test-equal "empty deque"
  '()
  (deque-items d))

(print-deque d)

(front-insert-deque! d 'a)
(test-equal "one element inserted from front in deque"
  '(a)
  (deque-items d))

(print-deque d)

(front-insert-deque! d 'b)
(test-equal "two elements inserted from front in deque"
  '(b a)
  (deque-items d))

(print-deque d)

(rear-delete-deque! d)
(test-equal "one element deleted from rear"
  '(b)
  (deque-items d))

(print-deque d)

(front-delete-deque! d)
(test-equal "one element deleted from front"
  '()
  (deque-items d))

(print-deque d)

(rear-insert-deque! d 'c)
(test-equal "one element inserted from rear"
  '(c)
  (deque-items d))

(print-deque d)

(rear-insert-deque! d 'd)
(test-equal "two elements inserted from rear"
  '(c d)
  (deque-items d))

(print-deque d)

(front-delete-deque! d)
(test-equal "one element deleted from front"
  '(d)
  (deque-items d))

(print-deque d)

(test-end "3.3.2.x23: message passing style implementation of deque")
