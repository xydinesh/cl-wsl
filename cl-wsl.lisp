;;;; cl-wsl.lisp

(in-package #:cl-wsl)

(defun generate-schedule()
  )

(defun build()
  (let ((i -1) 
	(country-table (make-hash-table :test #'equal))
	(distance-table nil)
	(distance-list nil)) 
    (with-open-file (stream "dist.txt")
      (loop for line = (read-line stream nil :eof)
	    until (eq line :eof)
	    do
	    (let ((d (split-sequence #\Space line :remove-empty-subseqs t)))
	      (push d distance-list)
	      (if (equal nil (gethash (first d) country-table))
		 (setf (gethash (first d) country-table) (incf i)))
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
      (format t "~a ~%" distance-table)))

(defun main()
     (build))
