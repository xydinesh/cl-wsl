;;;; cl-wsl.lisp

(in-package #:cl-wsl)

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)) (make-random-state t))))

(defun random-week()
  (let ((week (make-array '(12)))
	(m 0) (n 0) (value 0) (l 0) (teams nil)) 
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
  (let ((week nil) (schedule nil) (dup nil) (i 0) (j 0) (l 0) (carray nil))
    (setf schedule (make-array '(9 12)))
    (loop while (< i 9)
	  do
	  (setf week (random-week))
	  (setf l (length week))
	  (setf j 0)
	  (loop while (> l j)
		do
		(setf (aref schedule i j) (elt week j))
		(incf j))
	  ;(format t "week ~a: ~a ~%" i week)
	  (incf i))
    schedule))

;; Calculate cost for dups
(defun calculate-cost-dups(schedule)
  (let ((i 0) (j 0) (cost 0) (v 0) (c-count 0) (teams nil))
    (loop while (> 12 i)
	  do
	  (setf teams (make-array '(12) :initial-element 0))
	  (setf j 0)
	  (setf c-count 0)
	  (loop while (> 9 j)
		do
		;(format t "i ~a j ~a v: ~a ~%" i j (aref teams j))
		; assign cost for duplicates
		(setf v (abs (aref schedule j i)))
		(if (/= v 100)
		  (progn
		    (if (/= (aref teams v) 0)
		      (incf cost 10000))
		    (setf (aref teams v) 1))
		  (incf c-count))
		(incf j))
	  (if (= c-count 0)
	    (incf cost 100000))
	  (if (> c-count 1)
	    (incf cost (* c-count 100000)))
	  (incf i))
    cost))

(defun calculate-cost(schedule)
  (let ((i 0) (j 0) (cost 0) (v 0) (teams nil))
    (incf cost (calculate-cost-dups schedule))
    cost))

(defun dedup-column(schedule)
  (let ((i 0) (cost 0) (v 0) (teams nil)
	      (v1 0) (v2 0) (v3 0) (v4 0))
    (setf teams (make-array '(12) :initial-element 0))
    (setf j (random-from-range 0 11))
	(loop while (> 9 i)
	      do
	      (setf v (abs (aref schedule i j)))
	      (if (/= v 100)
		(progn
		  (if (/= (aref teams v) 0)
		    (progn
		      (setf v1 (aref schedule i j))
		      (setf v2 (aref schedule i v))
		      (setf m (random-from-range 0 11))
		      (loop while (= j m)
			    do
			    (setf m (random-from-range 0 11)))
		      (setf v3 (aref schedule i m))
		      (if (/= (abs v3) 100)
			(progn
			  (setf v4 (aref schedule i (abs v3)))
			  ; Set new values
			  (setf (aref schedule i j) (abs v3))
			  (setf (aref schedule i v) (* -1 (abs v4)))
			  (setf (aref schedule i m) (abs v1))
			  (setf (aref schedule i (abs v3)) (* -1 (abs v2)))))))
		      (setf (aref teams v) 1)))
	      (incf i))
	schedule))


(defun swap-column(schedule)
  (let ((i 0) (j 0) (m 0)
	      (v1 0) (v2 0) (v3 0) (v4 0))
    (setf i (random-from-range 0 8))
    (setf j (random-from-range 0 11))
    (setf v1 (aref schedule i j))
    (if (/= (abs v1) 100)
      (progn
	(setf v2 (aref schedule i (abs v1)))
	(loop while (= j m)
	      do
	      (setf m (random-from-range 0 11)))
	(setf v3 (aref schedule i m))
	(if (/= (abs v3) 100)
	  (progn
	    (setf v4 (aref schedule i (abs v3)))
	    (setf (aref schedule i (abs v1)) (* -1 (abs v4)))
	    (setf (aref schedule i j) (abs v3))
	    (setf (aref schedule i m) (abs v1))
	    (setf (aref schedule i (abs v3)) (* -1 (abs v2)))))))
    schedule))

(defun add-bye(schedule)
  (let ((v1 0) (v2 0) (v 0))
    (setf i (random-from-range 0 8))
    (setf j (random-from-range 0 11))
    (setf v1 (aref schedule i j))
    (if (/= (abs v1) 100)
      (progn
	(setf (aref schedule i (abs v1)) 100)
	(setf (aref schedule i j) 100)))
    schedule))

(defun modify-schedule (schedule)
  (setf schedule (dedup-column schedule))
  ;; (setf schedule (swap-column schedule))
  (setf schedule (add-bye schedule))
  schedule)

;; Optimize schedule
(defun generate-schedule()
  (let ((week nil) (schedule nil) (i 0) (j 0) (l 0) (cost 0) (nschedule nil))
    (setf schedule (random-schedule))
    (format t "schedule-cost: ~a ~%" (calculate-cost schedule))
    (setf cost (calculate-cost schedule))
    (loop while ( > 10000 i)
	  do
	  (setf nschedule (dedup-column schedule))
	  (if (> cost (calculate-cost nschedule))
	    (progn
	      (setf cost (calculate-cost nschedule))
	      (format t "schedule: ~a cost ~a i ~a ~%" nschedule cost i)
	      (setf schedule nschedule)))
	  (setf nschedule (add-bye schedule))
	  (if (> cost (calculate-cost nschedule))
	    (progn
	      (setf cost (calculate-cost nschedule))
	      (format t "schedule: ~a cost ~a i ~a ~%" nschedule cost i)
	      (setf schedule nschedule)))
	  (setf nschedule (swap-column schedule))
	  (if (> cost (calculate-cost nschedule))
	    (progn
	      (setf cost (calculate-cost nschedule))
	      (format t "schedule: ~a cost ~a i ~a ~%" nschedule cost i)
	      (setf schedule nschedule)))
	  (incf i))))

(defun build()
  (let ((i -1) 
	(country-table (make-hash-table :test #'equal))
	(distance-table nil)
	(distance-list nil)) 
    (with-open-file (stream "dist.txt")
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

(defun main()
     (build))
