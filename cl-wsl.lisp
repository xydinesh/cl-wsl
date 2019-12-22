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

(defun generate-schedule()
  (let ((week nil) (schedule nil) (i 0) (j 0) (l 0))
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
	  (incf i)
	  (format t "week ~a: ~a ~%" i week))
    (format t "schedule: ~a ~%" schedule)))

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
    (generate-schedule)
    (format t "~a ~%" distance-table)))

(defun main()
     (build))
