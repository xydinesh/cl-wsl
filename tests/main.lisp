(defpackage cl-wsl/tests/main
  (:use :cl
        :cl-wsl
        :rove))

(in-package :cl-wsl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-wsl)' in your Lisp.
(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-random-week
  (testing "return a list with random numbers"
    (ok (= (length (random-week)) 12))))

(deftest test-random-schedule
  (testing "random schedule test"
    (ok (= (array-rank (random-schedule)) 2))
    (ok (equal (array-dimensions (random-schedule)) '(9 12)))))

(deftest test-byes-schedule
  (let ((sched (random-schedule)) (i 0))
    (testing "bye in random schedule"
      (ok (> (aref sched 8 0) 100))
      (ok (> (aref sched 8 1) 100))
      (ok (> (aref sched 8 2) 100))
      (ok (= (array-rank sched) 2)))))
