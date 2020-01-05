;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

(add-to-load-path
 (string-append (dirname (dirname (current-filename)))
                "/src/3.3/3.3.2.x21-23.Representing_queues"))

(use-modules (procedural-queue))

(test-begin "3.3.2.procedural-queue-module-test")

(define q (make-queue))

(test-assert "new queue is empty"
  (empty-queue? q))

(insert-queue! q 'a)
(test-equal "one element in queue"
  '(a)
  (front-ptr q))

(insert-queue! q 'b)
(test-equal "two elements in queue"
  '(a b)
  (front-ptr q))

(delete-queue! q)
(test-equal "first element deleted"
  '(b)
  (front-ptr q))

(insert-queue! q 'c)
(test-equal "another element queued"
  '(b c)
  (front-ptr q))

(insert-queue! q 'd)
(test-equal "another element queued"
  '(b c d)
  (front-ptr q))

(delete-queue! q)
(test-equal "current first element deleted"
  '(c d)
  (front-ptr q))

(test-end "3.3.2.procedural-queue-module-test")
