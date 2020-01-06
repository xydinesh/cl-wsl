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
  (let ((sched (random-schedule)))
    (testing "bye in random schedule"
      (dotimes (i 12)
	(ok (>= (aref sched 8 i) 100))))))

;;; We will be using this fixed schedule for generating more specific tests
(defun test-sched()
  #2A((8 7 -6 10 11 9 2 -1 0 -5 -3 -4)
      (-11 9 -7 -5 -6 3 4 2 -10 -1 8 0)
      (4 -11 -3 2 0 -8 -9 10 5 6 -7 1)
      (-2 -3 0 1 -10 11 -8 -9 6 7 4 -5)
      (4 -6 5 9 0 -2 1 -11 -10 -3 8 7)
      (9 -10 -3 2 -11 7 8 -5 -6 0 1 4)
      (-6 3 11 -1 7 -9 0 -4 10 5 -8 -2)
      (1 0 9 6 -11 7 -3 -5 10 -2 -8 4)
      (103 105 111 100 107 101 109 104 110 106 108 102)))

(deftest test-sched
  (let ((sched (test-sched)))
    (testing "creating fixed schedule for tests"
      (ok (= (array-rank sched) 2))
      (ok (equal (array-dimensions sched) '(9 12)))
      (dotimes (i 12)
	(ok (>= (aref sched 8 i)))))))
