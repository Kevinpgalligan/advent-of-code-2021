;; Algorithm:
;;   We have incorporated scanners S0, S1, ..., Sk-1 into the reference
;;   frame of S0. We want to add Sk.
;;   Try all rotations of Sk and form a comparison table with S0 for each rotation.
;;   If a rotation moves Sk into S0's reference frame, and S0 and Sk have N>=12 beacons in
;;   common, then the same offset should appear N times in the comparison
;;   table.
;;   If no rotation meets that criteria, then these beacons must not have
;;   enough beacons in common! Push Sk to the back of the list and on to the next one.
;;   Also need to check that it's consistent with all scanners processed so far.
;;   E.g. do S0, S1, ..., Sk-1 detect all the points that they should, and whatnot?
;;   If more than 1 rotation meets the criteria... error, my assumption is wrong.
;;   Remaining questions:
;;     1) Do we need to keep S0, S1, ..., Sk-1 separate, or can the lists of points be merged?
;;        I'd be inclined to merge them, if possible, so we can do a simple (length list) at
;;        the end to get the answer.
;;     2) Are false positive matches possible?
;;     3) Can multiple possible candidate offsets appear for a single rotation? ERROR.
;;     4) Can there be multiple candidate rotations?

(defclass point ()
  ((x
    :initarg :x
    :reader x)
   (y
    :initarg :y
    :reader y)
   (z
    :initarg :z
    :reader z)))

(defun make-point (x y z)
  (make-instance 'point :x x :y y :z z))

(defun make-translate (dx dy dz)
  (lambda (P)
    (make-point (+ dx (x P))
		(+ dy (y P))
		(+ dz (z P)))))

(defun make-rotate ()
  ;; TODO
  )

(defun all-rotations ()
  ;; TODO
  )

(defun load-scanners (path)
  (with-open-file (in path)
    (loop for scanner = (read-scanner in)
	  while scanner
	  collect scanner)))

(defun read-scanner (in)
  (if (read-line in nil nil)
      (loop for point = (read-point in)
	    while point
	    collect point)
      nil))

(defun read-point (in)
  (let ((line (read-line in nil nil)))
    (if (or (null line) (string= line ""))
	nil
	(destructuring-bind (x y z)
	    (mapcar #'parse-integer (split-string "," line))
	  (make-point x y z)))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun comparison-table (s1 s2)
  (let ((table (make-array (list (length s1)
				 (length s2))
			   :initial-element nil)))
    (loop for i = 0 then (1+ i)
	  for p1 in s1
	  do (loop for j = 0 then (1+ j)
		   for p2 in s2
		   do (setf (aref table i j) (P- p1 p2))))
    table))

(defun P- (p1 p2)
  (make-point (- (x p1) (x p2)) (- (y p1) (y p2)) (- (z p1) (z p2))))

(defun printable-table (table)
  (loop for i below (car (array-dimensions table))
	collect (loop for j below (cadr (array-dimensions table))
		      collect (let ((P (aref table i j)))
				(list (x P) (y P) (z P))))))
