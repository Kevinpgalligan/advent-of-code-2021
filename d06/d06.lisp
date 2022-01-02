(defparameter *new-fish-age* 8)
(defparameter *reset-fish-age* 6)
(defparameter *max-age* (max *new-fish-age* *reset-fish-age*))

(defun read-lanternfish-ages (path)
  (with-open-file (in path)
    (mapcar #'parse-integer
	    (split-string "," (read-line in)))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun calculate-lanternfish-population (ages days)
  (let ((calendar (make-array (1+ *max-age*) :initial-element 0)))
    (loop for age in ages
	  do (incf (aref calendar age)))
    (loop repeat days
	  do (let ((num-births (aref calendar 0)))
	       (loop for i from 1 to *max-age*
		     do (setf (aref calendar (1- i)) (aref calendar i)))
	       (setf (aref calendar *new-fish-age*) num-births)
	       (incf (aref calendar *reset-fish-age*) num-births)))
    (loop for i from 0 to *max-age*
	  sum (aref calendar i))))
