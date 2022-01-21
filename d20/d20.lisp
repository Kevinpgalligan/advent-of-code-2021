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

(defun evolve (live-set algorithm)
  (let ((dead-cells (make-coord-set))
	(new-live-set (make-coord-set)))
    (loop for c in (get-all-coords live-set)
	  do (let ((neighbours (get-neighbours c)))
	       (when (should-live-p neighbours algorithm live-set)
		 (put-coord new-live-set c))
	       (loop for n in neighbours
		     do (when (not (containsp live-set n))
			  (put-coord dead-cells n)))))
    (loop for c in (get-all-coords dead-cells)
	  do (let ((neighbours (get-neighbours c)))
	       (when (should-live-p neighbours algorithm live-set)
		 (put-coord new-live-set c))))
    new-live-set))

(defun should-live-p (neighbours algorithm live-set)
  (aref algorithm (neighbours->index neighbours live-set)))

(defun neighbours->index (neighbours live-set)
  (let ((index 0))
    (loop for n in neighbours
	  for j = 8 then (1- j)
	  do (when (containsp live-set n)
	       (setf index (logand index (ash 1 j)))))
    index))

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
  (loop repeat iterations
	do (setf live-set (evolve live-set algorithm)))
  (coord-set-size live-set))
