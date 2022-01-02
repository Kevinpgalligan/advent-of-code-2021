(defun read-instructions (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (let ((split-index (search " " line)))
		    (cons (subseq line 0 split-index)
			  (parse-integer (subseq line (1+ split-index))))))))

(defun calculate-position (instructions)
  (let ((horizontal-position 0)
	(depth 0)
	(aim 0))
    (loop for (direction . units) in instructions
	  do (cond
	       ((string= direction "forward")
		(progn
		  (incf horizontal-position units)
		  (incf depth (* aim units))))
	       ((string= direction "down")
		(incf aim units))
	       ((string= direction "up")
		(decf aim units))
	       (t (error "unknown direction!"))))
    (list horizontal-position depth)))
