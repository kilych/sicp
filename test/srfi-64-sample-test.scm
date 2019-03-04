;; Load SRFI-64 lightweight testing specification
(use-modules (srfi srfi-64))
(use-modules (srfi srfi-1))

;; Suppress log file output. To write logs, comment out the following line:
(module-define! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)

;; To specify log file, uncomment the following lines:
;; (module-define! (resolve-module '(srfi srfi-64))
;;                 'test-log-to-file
;;                 (string-append (current-filename ".log")))

;;; Begin test suite
(test-begin "srfi-64-sample-test")

(test-assert "test #t is true"
  #t)

(test-end "srfi-64-sample-test")
;;; End test suite
