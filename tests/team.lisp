(defpackage cl-wsl/tests/main
  (:use #:cl
        #:cl-wsl
        #:rove)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package :cl-wsl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-wsl)' in your Lisp.
(deftest test-swap-schedule
  (let ((sched (test-sched)) (new-sched nil))
    (testing "swap teams tests"
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 1 5) 3))
      (setf new-sched (swap-teams sched 1 1 5))
      (ok (= (aref new-sched 1 1) 3))
      (ok (= (aref new-sched 1 9) -5))
      (ok (= (aref new-sched 1 5) 9))
      (ok (= (aref new-sched 1 3) -1)))

    (testing "swap teams tests with optional args"
      (setq sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 1 5) 3))
      (setf new-sched (swap-teams sched 1 1 5))
      (ng (equal (array-cmp (test-sched) new-sched) t)))

    (testing "swap teams tests with optional args 1"
      (setq sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 1 5) 3))
      (setf new-sched (swap-teams sched 1 1))
      (ng (equal (array-cmp (test-sched) new-sched) t)))

    (testing "swap teams tests with optional args 2"
      (setq sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 1 5) 3))
      (setf new-sched (swap-teams sched 1))
      (ng (equal (array-cmp (test-sched) new-sched) t)))

    (testing "swap teams tests with optional args 3"
      (setq sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 1 5) 3))
      (setf new-sched (swap-teams sched))
      (ng (equal (array-cmp (test-sched) new-sched) t)))))



(deftest test-swap-team-byes
  (let ((sched (test-sched)) (new-sched nil))
    (testing "swap team byes tests"
      (ok (= (aref sched 8 1) 105))
      (ok (= (aref sched 8 5) 101))
      (ok (= (aref sched 8 9) 106))
      (setf new-sched (swap-teams sched 8 1 9))
      (ok (= (aref new-sched 8 1) 106))
      (ok (= (aref new-sched 8 9) 105))
      (ok (= (aref new-sched 8 5) 109))
      (ok (= (aref new-sched 8 6) 101)))

    (testing "swap teams byes tests with optional args 1"
      (setq sched (test-sched))
      (setf new-sched (swap-teams sched 8 1))
      (ng (equal (array-cmp (test-sched) new-sched) t)))

    (testing "swap teams byes tests with optional args 2"
      (setq sched (test-sched))
      (setf new-sched (swap-teams sched 8))
      (ng (equal (array-cmp (test-sched) new-sched) t)))))

(deftest test-swap-team-byes-1
  (let ((sched (test-sched-1)) (new-sched nil))
    (testing "swap team byes tests 1"
      (ok (= (aref sched 3 1) 103))
      (ok (= (aref sched 3 3) 101))
      (ok (= (aref sched 3 5) 11))
      (ok (= (aref sched 3 11) -5))
      ;;(-2 103 0 101 -10 11 -8 -9 6 7 4 -5)
      (setf new-sched (swap-teams sched 3 1 5))
      ;;(-2 111 0 -5 -10 3 -8 -9 6 7 4 101)
      (ok (= (aref new-sched 3 1) 111))
      (ok (= (aref new-sched 3 3) -5))
      (ok (= (aref new-sched 3 5) 3))
      (ok (= (aref new-sched 3 11) 101)))
    (testing "swap team byes tests 2"
      (setq sched (test-sched-1))
      (ok (= (aref sched 3 1) 103))
      (ok (= (aref sched 3 4) -10))
      (ok (= (aref sched 3 10) 4))
      (ok (= (aref sched 3 3) 101))
      ;;(-2 103 0 101 -10 11 -8 -9 6 7 4 -5)
      (setf new-sched (swap-teams sched 3 4 1))
      ;;(-2 10 0 104 103 11 -8 -9 6 7 -1 -5)
      (ok (= (aref new-sched 3 1) 10))
      (ok (= (aref new-sched 3 3) 104))
      (ok (= (aref new-sched 3 4) 103))
      (ok (= (aref new-sched 3 10) -1)))
    (testing "swap team byes tests 3"
      (setq sched (test-sched-1))
      (ok (= (aref sched 4 1) 106))
      (ok (= (aref sched 4 6) 101))
      (ok (= (aref sched 4 2) 105))
      (ok (= (aref sched 4 5) 102))
      ;;(4 106 105 9 0 102 101 -11 -10 -3 8 7)
      (setf new-sched (swap-teams sched 4 1 2))
      ;;(4 105 106 9 0 101 102 -11 -10 -3 8 7)
      (ok (= (aref new-sched 4 1) 105))
      (ok (= (aref new-sched 4 2) 106))
      (ok (= (aref new-sched 4 5) 101))
      (ok (= (aref new-sched 4 6) 102)))
    (testing "swap team byes tests 3"
      (setq sched (test-sched-1))
      (ok (= (aref sched 4 1) 106))
      (ok (= (aref sched 4 6) 101))
      (ok (= (aref sched 4 11) 7))
      (ok (= (aref sched 4 7) -11))
      ;;(4 106 105 9 0 102 101 -11 -10 -3 8 7)
      (setf new-sched (swap-teams sched 4 1 11))
      (ok (= (aref new-sched 4 1) 107))
      (ok (= (aref new-sched 4 6) -11))
      (ok (= (aref new-sched 4 7) 101))
      (ok (= (aref new-sched 4 11) 6)))
    (testing "swap team byes tests 4"
      (setq sched (test-sched-1))
      (ok (= (aref sched 4 1) 106))
      (ok (= (aref sched 4 6) 101))
      (ok (= (aref sched 4 11) 7))
      (ok (= (aref sched 4 7) -11))
      ;;(4 106 105 9 0 102 101 -11 -10 -3 8 7)
      (setf new-sched (swap-teams sched 4 11 1))
      ;;(4 7 105 9 0 102 111 -1 -10 -3 8 106)
      (ok (= (aref new-sched 4 1) 7))
      (ok (= (aref new-sched 4 6) 111))
      (ok (= (aref new-sched 4 7) -1))
      (ok (= (aref new-sched 4 11) 106)))
    (testing "swap team byes tests 5"
      (setq sched (test-sched-1))
      (ok (= (aref sched 4 3) 9))
      (ok (= (aref sched 4 9) -3))
      (ok (= (aref sched 4 11) 7))
      (ok (= (aref sched 4 7) -11))
      ;;(4 106 105 9 0 102 101 -11 -10 -3 8 7)
      (setf new-sched (swap-teams sched 4 11 3))
      ;;(4 106 105 7 0 102 101 -3 -10 -11 8 9)
      (ok (= (aref new-sched 4 3) 7))
      (ok (= (aref new-sched 4 7) -3))
      (ok (= (aref new-sched 4 9) -11))
      (ok (= (aref new-sched 4 11) 9)))    
    ))

