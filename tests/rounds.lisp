(defpackage cl-wsl/tests/main
  (:use #:cl
        #:cl-wsl
        #:rove)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package :cl-wsl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-wsl)' in your Lisp.
(deftest test-swap-rounds
  (let ((sched (test-sched)) (new-sched nil))
    (testing "swap rounds eams tests"
      (fail "Working on swapping rounds"))))

