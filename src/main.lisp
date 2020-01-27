;;;; cl-wsl.lisp
(defpackage cl-wsl
  (:use :cl)
  (:export
   :swap-teams
   :swap-rounds
   :random-schedule
   :random-week))

(in-package :cl-wsl)

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)) (make-random-state t))))

(defun random-week()
  (let ((week (make-array '(12)))
	(value-1 0) (value-2 0)
          (m 0) (n 0) (l 0) (teams nil)) 
    (dotimes (k 12)
      (push k teams))
    (setf teams (reverse teams))
    (setf l (length teams))
    (loop while (> l 0)
	  do
	  (setf m (random-from-range 0 (- l 1)))
	  (setf n (random-from-range 0 (- l 1)))
	  (if (/= m n)
	    (progn
	      (setf value-1 (elt teams m))
	      (setf value-2 (elt teams n))
	      (setf (aref week value-1) value-2)
	      (setf (aref week value-2) (* value-1 -1))
	      ;(format t "m ~a n ~a week ~a value-1 ~a value-2 ~a team ~a ~%" m n week value-1 value-2 teams)
	      (setf teams (remove value-1 teams :test 'equal))
	      (setf teams (remove value-2 teams :test 'equal))))
	      (setf l (length teams)))
    week))

;; Generate a random schedule
(defun random-schedule()
  (let ((week nil) (schedule nil) (i 0) (j 0) (l 0))
    (setf schedule (make-array '(9 12)))
    (loop while (< i 9)
	  do
	  (setf week (random-week))
	  (setf l (length week))
	  (setf j 0)
	  (loop while (> l j)
		do
		(if (= i 8)
		  (setf (aref schedule i j) (+ 100 (abs (elt week j))))
		  (setf (aref schedule i j) (elt week j)))
		(incf j))
	  ;(format t "week ~a: ~a ~%" i week)
	  (incf i))
    schedule))

;; Calculate cost for dups
(defun calculate-cost(schedule)
  (let ((i 0) (j 0) (cost 0) (dvalue 0) (v 0)  (teams nil))
    (loop while (> 12 i)
	  do
	  (setf teams (make-array '(112) :initial-element 0))
	  (setf j 0)
	  (loop while (> 9 j)
		do
		;(format t "i ~a j ~a v: ~a ~%" i j (aref teams j))
		; assign cost for duplicates
		(setf v (abs (aref schedule j i)))
		(setf dvalue (aref teams v))
		(setf (aref teams v) (incf dvalue))
		(if (> dvalue 1)
		  (incf cost (* (expt 10 dvalue) 100)))
		(incf j))
	  (incf i))
    cost))

(defun swap-rounds (schedule &optional i j m)
  (let ((teams nil)
	(v1 0) (v2 0) (v3 0) (v4 0) (v5 0) (v6 0)
	(tmp 0) (byes-1 nil) (byes-2 nil) (byes-3 nil) (byes-4 nil))

    (dotimes (k 8)
      (push k teams))

    (if (not i)
	(setf i (random-from-range 0 11)))

    (if (not j)
	(setf j (random-from-range 0 8)))

    (setf teams (remove j teams :test 'equal))
    (if (not m)
	(setf m (elt teams (random-from-range 0 (- (length teams) 1)))))
    (format t "i: ~a j: ~a m: ~a ~%" i j m)

    (setf v1 (aref schedule i j))
    (if (>= v1 100)
	(progn
	  (setf byes-1 t)
	  (setf v1 (- v1 100))))
    
    (setf v2 (aref schedule i (abs v1)))
    (if (>= v2 100)
	(setf v2 (- v2 100)))
    
    (setf v3 (aref schedule m j))
    (if (>= v3 100)
	(progn
	  (setf byes-2 t)
	  (setf v3 (- v3 100))))

    
    (setf v4 (aref schedule i (abs v3)))
    (if (>= v4 100)
	(progn
	  (setf byes-3 t)
	  (setf v4 (- v4 100))))
    
    (setf v5 (aref schedule m (abs v1)))
    (if (>= v5 100)
	(progn
	  (setf byes-4 t)
	  (setf v5 (- v5 100))))
    
    (setf v6 (aref schedule m (abs v3)))
    (if (>= v6 100)
	(setf v6 (- v6 100)))
    
    (format t "v1: ~a v2: ~a v3: ~a v4: ~a v5: ~a ~%" v1 v2 v3 v4 v5)

    ;; swap values i,j <-> m,j
    (setf tmp (aref schedule i j))
    (setf (aref schedule i j) (aref schedule m j))
    (setf (aref schedule m j) tmp)

    ;; set i,v3 and m,v1
    (setf (aref schedule i (abs v3))
	  (if byes-2
	      (+ 100 (abs v6))
	      v6))
    
    (setf (aref schedule m (abs v1))
	  (if byes-1
	      (+ 100 (abs v2))
	      v2))

    ;; set i,v1 = v4 i,v4 = v1
    (setf (aref schedule i (abs v1))
	  (if byes-3
	      (+ 100 (abs v4))
	      (abs v4)))

    (setf (aref schedule i (abs v4))
	  (if byes-3
	      (+ 100 (abs v1))
	      (* -1 (abs v1))))

    ;; set m,v3 = v5 and m,v5 = v3
    (setf (aref schedule m (abs v3))
	  (if byes-4
	      (+ 100 (abs v5))
	      (abs v5)))
    
    (setf (aref schedule m (abs v5))
	  (if byes-4
	      (+ 100 (abs v3))
	      (* -1 (abs v3))))
    schedule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swap teams in a given week
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-teams(schedule &optional i j m)
  (let ((teams nil) (v1 0) (v2 0) (v3 0) (v4 0) (byes-1 nil) (byes-2 nil))

    (if (not i)
	(setf i (random-from-range 0 8)))

    (if (not j)
	(setf j (random-from-range 0 11)))

    (setf v1 (aref schedule i j))
    ;; Byes marked as 100+ values
    (if (>= v1 100)
	(progn 
	  (setf byes-1 t)
	  (setf v1 (- v1 100))))
    
    (setf v2 (aref schedule i (abs v1)))
    (if byes-1
	(setf v2 (- v2 100)))

    ;; make sure we don't get duplicate values
    (dotimes (k 12)
      (push k teams))
    (setf teams (remove (abs v1) teams :test 'equal))
    (setf teams (remove j teams :test 'equal))
    
    (if (not m)
	(setf m (elt teams (random-from-range 0 (- (length teams) 1)))))

    (setf v3 (aref schedule i m))
    (if (>= v3 100)
	(progn 
	  (setf byes-2 t)
	  (setf v3 (- v3 100))))

    (setf v4 (aref schedule i (abs v3)))
    (if byes-2
	(setf v4 (- v4 100)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Swap Values
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf (aref schedule i j)
	  (if byes-2
	      (+ 100 v3)
	      (if byes-1
		  (+ 100 (abs v3))
		  (abs v3))))
    
    (setf (aref schedule i (abs v1))
	  (if (and byes-2 byes-1)
	      (+ 100 v4)
	      (* -1 (abs v4))))
    
    (setf (aref schedule i m)
	  (if (and byes-2 byes-1)
	      (+ 100 (abs v1))
	      (abs v1)))

    (setf (aref schedule i (abs v3))
	  (if byes-1
	      (+ 100 v2)
	      (if byes-2
		  (+ 100 (abs v2))
		  (* -1 (abs v2)))))
    schedule))

(defun modify-schedule(schedule)
  (let ((i 0))
    (setf i (random-from-range 1 2))
    (cond
      ((= i 2) (swap-rounds schedule))
      ((= i 1) (swap-teams schedule)))))
;; Optimize schedule
(defun generate-schedule()
  (let ((schedule nil) (i 0) 
	(cost-1 0) (u 0) (delta 0) (temp 0) (beta 0)
	(accept 0)
	(best-schedule 0)
	(best-cost 0)
	(cost 0) (nschedule nil))
    (setf schedule (random-schedule))
    (format t "schedule-cost: ~a ~%" (calculate-cost schedule))
    (setf cost (calculate-cost schedule))
    (setf best-cost cost)
    (setf cost-1  cost)
    (setf temp 1000)
    (setf beta 0.999)
    (loop while ( > 200000 i)
	  do
	  (setf accept 0)
	  (setf nschedule (modify-schedule schedule))
	  (setf cost-1 (calculate-cost nschedule))
	  (setf delta (- cost-1 cost))
	  (setf u (- cost (* temp (log (random 1.0) 10))))
	  ;(format t "cost-1 ~a delta ~a u ~a temp ~a ~%" cost-1 delta u temp)
	  (if (> 0 delta)
	    (progn
	      (setf cost cost-1)
	      ;;(format t "schedule: ~a cost ~a i ~a ~%" nschedule cost i)
	      (setf accept 1)
	      (setf schedule nschedule))
	    (if (> u cost-1)
	    (progn
	      (setf cost cost-1)
	      ;;(format t "u schedule: ~a cost ~a i ~a ~%" nschedule cost i)
	      (setf schedule nschedule))))
	  (if (= 1 accept)
	      (if (> best-cost cost)
		(progn
		  (format t "schedule: ~a cost ~a i ~a ~%" schedule cost i)
		  (setf best-schedule schedule)
		  (setf best-cost cost))))
	  (setf temp (* temp beta))
	  (incf i))))

(defun build()
  (let ((i -1) 
	(country-table (make-hash-table :test #'equal))
	(distance-table nil)
	(distance-list nil)) 
    (with-open-file (stream "data/dist.txt")
      ; read file line by line until the end
      (loop for line = (read-line stream nil :eof)
	    until (eq line :eof)
	    do
	    ; split line by spaces
	    (let ((d (split-sequence #\Space line :remove-empty-subseqs t)))
	      (push d distance-list)
	      ; line has the format A B x
	      ; First we add A to country-table if it is already not in the table
	      (if (equal nil (gethash (first d) country-table))
		 (setf (gethash (first d) country-table) (incf i)))
	      ; Add B to country-table if it is not in the table
	      (if (equal nil (gethash (second d) country-table))
		(setf (gethash (second d) country-table) (incf i))))))
    (let ((hcount 0))
      (setf hcount (hash-table-count country-table))
      (setf distance-table (make-array (list hcount hcount)))
      (dolist (item distance-list)
	(setf (aref distance-table 
		    (gethash (first item) country-table) 
		    (gethash (second item) country-table))
	      (parse-integer (third item)))))
    ;(format t "~a ~%" distance-table)
    (generate-schedule)))
