(defun read-vent-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (apply #'make-line
			 (mapcar #'parse-point (split-string " -> " line))))))

(defun parse-point (string)
  (destructuring-bind (x y)
      (mapcar #'parse-integer (split-string "," string))
    (make-point x y)))

(defun make-point (x y) (list x y))
(defun point-x (point) (car point))
(defun point-y (point) (cadr point))

(defun make-line (p1 p2) (list p1 p2))
(defun line-p1 (line) (car line))
(defun line-p2 (line) (cadr line))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun count-dangerous-areas (vent-lines &key include-diagonal)
  (let* ((width (1+
		 (loop for line in vent-lines
		       maximize (max (point-x (line-p1 line))
				     (point-x (line-p2 line))))))
	 (height (1+
		  (loop for line in vent-lines
			maximize (max (point-y (line-p1 line))
				      (point-y (line-p2 line))))))
	 (map (make-array (list height width) :initial-element 0)))
    (loop for line in vent-lines
	  do (let* ((x1 (point-x (line-p1 line)))
		    (x2 (point-x (line-p2 line)))
		    (y1 (point-y (line-p1 line)))
		    (y2 (point-y (line-p2 line)))
		    (dx (signum (- x2 x1)))
		    (dy (signum (- y2 y1))))
	       (when (or include-diagonal (zerop dx) (zerop dy))
		 (loop for x = x1 then (+ x dx)
		       for y = y1 then (+ y dy)
		       do (incf (aref map y x))
		       while (or (not (= x x2))
				 (not (= y y2)))))))
    (loop for i below height
	  sum (loop for j below width
		    when (>= (aref map i j) 2)
		      sum 1))))
