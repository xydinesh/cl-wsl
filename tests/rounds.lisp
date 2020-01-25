(defpackage cl-wsl/tests/main
  (:use #:cl
        #:cl-wsl
        #:rove)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package :cl-wsl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-wsl)' in your Lisp.
;; '((8 7 -6 10 11 9 2 -1 0 -5 -3 -4)
;;   (-11 9 -7 -5 -6 3 4 2 -10 -1 8 0)
;;   (4 -11 -3 2 0 -8 -9 10 5 6 -7 1)
;;   (-2 -3 0 1 -10 11 -8 -9 6 7 4 -5)
;;   (4 -6 5 9 0 -2 1 -11 -10 -3 8 7)
;;   (9 -10 -3 2 -11 7 8 -5 -6 0 1 4)
;;   (-6 3 11 -1 7 -9 0 -4 10 5 -8 -2)
;;   (1 0 9 6 -11 7 -3 -5 10 -2 -8 4)
;;   (103 105 111 100 107 101 109 104 110 106 108 102))))

(deftest test-swap-rounds
  (let ((sched (test-sched)) (new-sched nil))
    (testing "swap round tests"
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 6 1) 3))
      (ok (= (aref sched 6 3) -1))
      (setf new-sched (swap-rounds sched 1 1 6))
      (ok (= (aref new-sched 6 1) 9))
      (ok (= (aref new-sched 6 3) -5))
      (ok (= (aref new-sched 6 5) 3))
      (ok (= (aref new-sched 6 9) -1))
      (ok (= (aref new-sched 1 1) 3))
      (ok (= (aref new-sched 1 9) 5))
      (ok (= (aref new-sched 1 5) -9))
      (ok (= (aref new-sched 1 3) -1)))
    ;;   (-11 9 -7 -5 -6 3 4 2 -10 -1 8 0)
    ;;   (103 105 111 100 107 101 109 104 110 106 108 102)
    (testing "swap round tests with byes"
      (setf sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 8 1) 105))
      (ok (= (aref sched 8 5) 101))
      (setf new-sched (swap-rounds sched 1 1 8))
      (ok (= (aref new-sched 8 1) 9))
      (ok (= (aref new-sched 8 5) 3))
      (ok (= (aref new-sched 8 6) 4))
      (ok (= (aref new-sched 8 9) -1))
      (ok (= (aref new-sched 1 1) 105))
      (ok (= (aref new-sched 1 5) 101))
      (ok (= (aref new-sched 1 6) 109))
      (ok (= (aref new-sched 1 9) 106)))
    (testing "swap round tests with byes 1"
      (setf sched (test-sched))
      (ok (= (aref sched 1 1) 9))
      (ok (= (aref sched 1 9) -1))
      (ok (= (aref sched 8 1) 105))
      (ok (= (aref sched 8 5) 101))
      (setf new-sched (swap-rounds sched 8 1 1))
      (ok (= (aref new-sched 8 1) 9))
      (ok (= (aref new-sched 8 5) 3))
      (ok (= (aref new-sched 8 6) 4))
      (ok (= (aref new-sched 8 9) -1))
      (ok (= (aref new-sched 1 1) 105))
      (ok (= (aref new-sched 1 5) 101))
      (ok (= (aref new-sched 1 6) 109))
      (ok (= (aref new-sched 1 9) 106)))))



