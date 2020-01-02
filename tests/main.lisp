(defpackage cl-wsl/tests/main
  (:use :cl
        :cl-wsl
        :rove))
(in-package :cl-wsl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-wsl)' in your Lisp.
(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))





