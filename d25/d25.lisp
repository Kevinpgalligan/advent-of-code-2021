(defun parse-seafloor (path)
  (let ((lines
	  (with-open-file (in path)
	    (loop for line = (read-line in nil nil)
		  while line
		  collect line))))
    (let ((map (make-array (list (length lines)
				 (length (first lines))))))
      (loop for line in lines
	    for row = 0 then (1+ row)
	    do (loop for c across line
		     for col = 0 then (1+ col)
		     do (setf (aref map row col)
			      (case c
				(#\> 'east)
				(#\v 'south)))))
      map)))

(defun solve (map)
  (loop for step = 1 then (1+ step)
	do (let ((moves 0))
	     (incf moves (advance! map 'east))
	     (incf moves (advance! map 'south))
	     (when (zerop moves)
	       (return step)))))

(defun advance! (map direction)
  (let ((moves 0))
    (loop for i below (first-dimension map direction)
	  do (let (cucs-to-move)
	       (loop for j below (second-dimension map direction)
		     do (let* ((next-j
				 (mod (1+ j) (second-dimension map direction))))
			  (when (and (eq direction (fetch map direction i j))
				     (not (fetch map direction i next-j)))
			    (push (list j next-j) cucs-to-move))))
	       (incf moves (length cucs-to-move))
	       ;; Reverse so that we don't accidentally overwrite a cuc.
	       (loop for (j next-j) in (reverse cucs-to-move)
		     do (setf (fetch map direction i next-j)
			      (fetch map direction i j))
		     do (setf (fetch map direction i j) nil))))
    moves))

(defun first-dimension (map direction)
  (get-dimension map direction #'first))

(defun second-dimension (map direction)
  (get-dimension map direction #'second))

(defun get-dimension (map direction which-dimension)
  (funcall
   (funcall which-dimension
	    (case direction
	      (east (list #'first #'second))
	      (south (list #'second #'first))))
   (array-dimensions map)))

(defun fetch (map direction i j)
  (if (eq direction 'east)
      (aref map i j)
      (aref map j i)))

(defun (setf fetch) (new-value map direction i j)
  (if (eq direction 'east)
      (setf (aref map i j) new-value)
      (setf (aref map j i) new-value)))
