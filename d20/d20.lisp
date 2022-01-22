(defun make-coord-set ()
  (make-hash-table :test 'equalp))

(defun coord-set-size (set)
  (hash-table-count set))

(defun put-coord (set c)
  (setf (gethash c set) t))

(defun get-all-coords (set)
  (loop for c being each hash-key of set
	collect c))

(defun containsp (set c)
  (gethash c set))

(defun coord (x y) (list x y))
(defun cx (xy) (first xy))
(defun cy (xy) (second xy))

(defun get-neighbours (xy)
  (loop for (dx dy) in '((-1 -1) (0 -1) (1 -1)
			 (-1 0) (0 0) (1 0)
			 (-1 1) (0 1) (1 1))
	collect (coord (+ dx (cx xy))
		       (+ dy (cy xy)))))

(defun evolve (live-set algorithm iterations)
  (let ((finite-set live-set)
	(finite-type 'live)
	(infinite-type 'dead))
    (loop repeat iterations
	  do (let ((adjacents (make-coord-set))
		   (new-finite-set (make-coord-set))
		   ;; The infinite set is going to switch state, we have to flip the
		   ;; type of cell we're keeping track of.
		   (new-finite-type (if (aref algorithm 0) infinite-type finite-type))
		   (new-infinite-type (if (aref algorithm 0) finite-type infinite-type)))
	       (loop for c in (get-all-coords finite-set)
		     do (let ((neighbours (get-neighbours c)))
			  (when (eq new-finite-type (calculate-new-state neighbours algorithm finite-set finite-type))
			    (put-coord new-finite-set c))
			  (loop for n in neighbours
				do (when (not (containsp finite-set n))
				     (put-coord adjacents n)))))
	       (loop for c in (get-all-coords adjacents)
		     when (eq new-finite-type (calculate-new-state (get-neighbours c) algorithm finite-set finite-type))
		       do (put-coord new-finite-set c))
	       (setf finite-set new-finite-set
		     finite-type new-finite-type
		     infinite-type new-infinite-type)))
    finite-set))

(defun calculate-new-state (neighbours algorithm finite-set finite-type)
  (if (aref algorithm (neighbours->index neighbours finite-set finite-type))
      'live
      'dead))

(defun neighbours->index (neighbours finite-set finite-type)
  (loop for n in neighbours
	for j = 8 then (1- j)
	when (let ((is-finite-type-p (containsp finite-set n)))
	       (or (and (eq 'live finite-type) is-finite-type-p)
		   (and (not (eq 'live finite-type)) (not is-finite-type-p))))
	  sum (ash 1 j)))

(defun load-input (path)
  (with-open-file (in path)
    (let ((algorithm (make-array 512 :initial-element nil)))
      (loop for c across (read-line in)
	    for i = 0 then (1+ i)
	    do (when (char= #\# c)
		 (setf (aref algorithm i) t)))
      (read-line in)
      (let ((live-set (make-coord-set)))
	(loop for line = (read-line in nil nil)
	      for y = 0 then (1+ y)
	      while line
	      do (loop for c across line
		       for x = 0 then (1+ x)
		       do (when (char= #\# c)
			    (put-coord live-set (coord x y)))))
	(values live-set algorithm)))))

(defun solve (live-set algorithm iterations)
  (evolve live-set algorithm iterations))
