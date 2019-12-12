;;;; cl-wsl.lisp

(in-package #:cl-wsl)

(defun build()
  (let ((i 0) 
	(country-table (make-hash-table :test #'equal)) 
	(distance-list nil)) 
    (with-open-file (stream "dist.txt")
      (loop for line = (read-line stream nil :eof)
	    until (eq line :eof)
	    do
	    (let ((d (split-sequence #\Space line :remove-empty-subseqs t)))
	      (push d distance-list)
	      (cond 
		((equal nil (gethash (first d) country-table))
		 (setf (gethash (first d) country-table) i) 
		 (format t "~a ~a ~r ~%" (first d) (second d) (parse-integer (third d)))
		 (setf i (+ i 1)))
		(t (format t "~a ~%" d)))))
      (format t "~a ~%" distance-list)))) 

(defun main()
     (build))
