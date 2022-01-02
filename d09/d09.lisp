(defun read-heightmap (path)
  (with-open-file (in path)
    (let ((heightmap-as-lists
	    (loop for line = (read-line in nil nil)
		  while line
		  collect (loop for c across line
				collect (digit-char-p c)))))
      (make-array
       (list (length heightmap-as-lists)
	     (length (car heightmap-as-lists)))
       :initial-contents heightmap-as-lists))))

(defun calculate-total-risk (heightmap)
  (loop for i below (heightmap-vertical-width heightmap)
	sum (loop for j below (heightmap-horizontal-width heightmap)
		  when (low-point? heightmap i j)
		    sum (1+ (aref heightmap i j)))))

(defun heightmap-vertical-width (heightmap)
  (car (array-dimensions heightmap)))

(defun heightmap-horizontal-width (heightmap)
  (cadr (array-dimensions heightmap)))

(defun low-point? (heightmap i j)
  (loop for (adj-i adj-j) in (adjacent-points heightmap i j)
	always (< (aref heightmap i j) (aref heightmap adj-i adj-j))))

(defun adjacent-points (heightmap i j)
  (remove-if
   (lambda (adj-coords)
     (destructuring-bind (adj-i adj-j)
	 adj-coords
       (or (< adj-i 0)
	   (< adj-j 0)
	   (>= adj-i (heightmap-vertical-width heightmap))
	   (>= adj-j (heightmap-horizontal-width heightmap)))))
   (loop for (di dj) in '((-1 0) (+1 0) (0 -1) (0 +1))
	 collect (list (+ i di) (+ j dj)))))

(defun solve-basins-problem (heightmap)
  (let ((visited (make-array (array-dimensions heightmap) :initial-element nil)))
    (reduce #'*
	    (subseq (sort
		     (apply #'nconc
			    (loop for i below (heightmap-vertical-width heightmap)
				  collect (loop for j below (heightmap-horizontal-width heightmap)
						when (and (not (= 9 (aref heightmap i j)))
							  (not (aref visited i j)))
						  collect (basin-size! heightmap visited i j))))
		     #'>)
		    0
		    3))))

(defun basin-size! (heightmap visited i j)
  (setf (aref visited i j) t)
  (1+
   (loop for (adj-i adj-j) in (adjacent-points heightmap i j)
	 when (and (not (aref visited adj-i adj-j))
		   ;; Each point is part of a single basin. So don't need
		   ;; to check what height it is, it MUST be part of this basin.
		   ;; As long as it's not a 9.
		   (not (= 9 (aref heightmap adj-i adj-j))))
	   sum (basin-size! heightmap visited adj-i adj-j))))
