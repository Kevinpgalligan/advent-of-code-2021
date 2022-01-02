(defun read-cavern (path)
  (with-open-file (in path)
    (let ((cavern-as-lists
	    (loop for line = (read-line in nil nil)
		  while line
		  collect (loop for c across line
				collect (digit-char-p c)))))
      (make-array (list (length cavern-as-lists)
			(length (car cavern-as-lists)))
		  :initial-contents cavern-as-lists))))

(defun cavern-height (cavern)
  (car (array-dimensions cavern)))

(defun cavern-width (cavern)
  (cadr (array-dimensions cavern)))

(defun count-flashes! (cavern steps)
  (let ((flashes 0))
    (loop repeat steps
	  do (incf flashes (flash-step! cavern)))
    flashes))

(defun flash-step! (cavern)
  (let ((flashes 0))
    (loop for i below (cavern-height cavern)
	  do (loop for j below (cavern-width cavern)
		   when (= 10 (incf (aref cavern i j)))
		     do (propagate-flash! cavern i j)))
    (loop for i below (cavern-height cavern)
	  do (loop for j below (cavern-width cavern)
		   when (< 9 (aref cavern i j))
		     do (progn
			  (setf (aref cavern i j) 0)
			  (incf flashes))))
    flashes))

(defun propagate-flash! (cavern i j)
  (loop for (di dj) in '((1 1) (1 0) (1 -1)
			 (0 1) (0 -1)
			 (-1 1) (-1 0) (-1 -1))
	do (let ((ni (+ i di))
		 (nj (+ j dj)))
	     (when (and (<= 0 ni (1- (cavern-height cavern)))
			(<= 0 nj (1- (cavern-width cavern)))
			(< (aref cavern ni nj) 10))
	       (incf (aref cavern ni nj))
	       (when (= (aref cavern ni nj) 10)
		 (propagate-flash! cavern ni nj))))))

(defun first-sync! (cavern)
  (loop for step = 1 then (1+ step)
	when (=  (flash-step! cavern) (* (cavern-width cavern) (cavern-height cavern)))
	  return step))

