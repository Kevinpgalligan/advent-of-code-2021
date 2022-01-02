(defun read-crab-positions (path)
  (with-open-file (in path)
    (mapcar #'parse-integer (split-string "," (read-line in)))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun calculate-minimum-fuel (positions)
  (let* ((max-position (loop for p in positions maximize p))
	 (counts (position-counts positions max-position)))
    (loop for i from 0 to max-position
	  for crabs-to-left = 0 then (+ crabs-to-left (aref counts (1- i)))
	  for crabs-to-right = (length positions) then (- crabs-to-right (aref counts (1- i)))
	  for cost = (reduce #'+ positions) then (+ cost crabs-to-left (- crabs-to-right))
	  minimize cost)))

(defun position-counts (positions max-position)
  (let ((counts (make-array (1+ max-position) :initial-element 0)))
    (loop for p in positions
	  do (incf (aref counts p)))
    counts))

(defun calculate-minimum-fuel-exp (positions)
  "Where fuel cost rises the further you travel.
This is an ugly brute force method."
  (let* ((max-position (loop for p in positions maximize p))
	 (counts (position-counts positions max-position)))
    (loop for i from 0 to max-position
	  minimize (loop for j from 0 to max-position
			 sum (let* ((distance (abs (- i j)))
				    (position-cost
				      (/ (* distance (1+ distance)) 2)))
			       (* position-cost (aref counts j)))))))
