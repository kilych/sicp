;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.2.x21-23.Representing_queues"))

(use-modules (queue))

(test-begin "3.3.2.x22: message passing style implementation of queue")

(define q (make-queue))

(test-assert "new queue is empty"
  (empty-queue? q))

(print-queue q)

(insert-queue! q 'a)
(test-equal "one element in queue"
  '(a)
  (queue-items q))

(print-queue q)

(insert-queue! q 'b)
(test-equal "two elements in queue"
  '(a b)
  (queue-items q))

(print-queue q)

(delete-queue! q)
(test-equal "first element deleted"
  '(b)
  (queue-items q))

(print-queue q)

(insert-queue! q 'c)
(test-equal "another element queued"
  '(b c)
  (queue-items q))

(print-queue q)

(insert-queue! q 'd)
(test-equal "another element queued"
  '(b c d)
  (queue-items q))

(print-queue q)

(delete-queue! q)
(test-equal "current first element deleted"
  '(c d)
  (queue-items q))

(print-queue q)

(test-end "3.3.2.x22: message passing style implementation of queue")
