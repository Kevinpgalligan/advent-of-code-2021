;; Debugging todo:
;;  (1) see if overlap can be found between each pair of scanners in sample

(defparameter *common-beacons-threshold* 12)

(defun make-rotate (rotation-spec)
  (lambda (P)
    (make-point (get-rotation-coord P (first rotation-spec))
		(get-rotation-coord P (second rotation-spec))
		(get-rotation-coord P (third rotation-spec)))))

(defun get-rotation-coord (P coord-spec)
  (if (atom coord-spec)
      (slot-value P coord-spec)
      (- (slot-value P (cadr coord-spec)))))

;; Yay, hard-coding!
(defparameter *all-rotations*
  (mapcar #'make-rotate
	  '((x y z) (x y (- z)) (x (- y) z) (x (- y) (- z))
	    ((- x) y z) ((- x) y (- z)) ((- x) (- y) z) ((- x) (- y) (- z))
	    (y x z) (y x (- z)) (y (- x) z) (y (- x) (- z))
	    ((- y) x z) ((- y) x (- z)) ((- y) (- x) z) ((- y) (- x) (- z))
	    (z y x) (z y (- x)) (z (- y) x) (z (- y) (- x))
	    ((- z) y x) ((- z) y (- x)) ((- z) (- y) x) ((- z) (- y) (- x)))))

(defun solve (Ss)
  (let ((final-map (list (pop Ss))))
    (loop while Ss
	  do (let* ((S (pop Ss))
		    (integrated-S (integrate-scanner S final-map)))
	       (if integrated-S
		   (push integrated-S final-map)
		   ;; Push it to the back of the list, integrate later.
		   (setf Ss (append Ss (list S))))
	       (format t "scanners left: ~a~%" (length Ss))))
    (length (unique-points (points-sorted (apply #'nconc final-map))))))

(defun integrate-scanner (S1 other-scanners)
  (let ((S1-orientations (all-rotations-of-scanner S1)))
    (block result
      (loop for S2 in other-scanners
	    do (let ((shifts
		       (remove-if #'null
				  (loop for S1-rotated in S1-orientations
					collect (shift-to-reference-frame S2 S1-rotated)))))
		 (cond
		   ;; Try to match it with another scanner!
		   ((null shifts) nil)
		   ;; Managed to integrate it!
		   ((= 1 (length shifts))
		    (return-from result (car shifts)))
		   ;; Uh oh, I didn't think this would happen!
		   (t (error "Multiple viable rotations for a single scanner pair."))))))))

(defun all-rotations-of-scanner (S)
  (loop for rotation in *all-rotations*
	collect (scanner-apply-function S rotation)))

(defun shift-to-reference-frame (S2 S1)
  "Shift S1 to the reference frame of S2, if possible. Otherwise return nil."
  (let ((shift-translation (find-shift-translation (comparison-table S2 S1))))
    (when shift-translation
      (scanner-apply-function S1 shift-translation))))

(defun find-shift-translation (table)
  "Given a comparison table of 2 scanners, returns the translation needed to shift the 2nd scanner
to the frame of reference of the first (the offset that gives at least 12 matching beacons).
If there is no such offset, returns nil.
If there are multiple viable offsets, errors out."
  (let ((offset-counts (calculate-offset-counts table)))

    (let ((viable-offsets
	    (loop for offset being each hash-key of offset-counts using (hash-value count)
		  when (>= count *common-beacons-threshold*)
		    collect offset)))
      (cond
	((null viable-offsets) nil)
	((= 1 (length viable-offsets))
	 (apply #'make-translate (car viable-offsets)))
	(t (error "Multiple viable offsets for a single rotation."))))))

(defun calculate-offset-counts (table)
  (let ((offset-counts (make-hash-table :test 'equalp)))
    (loop for i below (car (array-dimensions table))
	  collect (loop for j below (cadr (array-dimensions table))
			collect (let ((offset (aref table i j)))
				  (setf (gethash offset offset-counts)
					(1+ (or (gethash offset offset-counts)
						0))))))
    offset-counts))

(defun scanner-apply-function (S f)
  (mapcar f S))

(defun points-sorted (Ps)
  ;; I'm sure there's a less braindead way to write this.
  (sort (mapcar #'copy-point Ps)
	(lambda (P1 P2)
	  (or (< (x P1) (x P2))
	      (and (= (x P1) (x P2))
		   (< (y P1) (y P2)))
	      (and (= (x P1) (x P2))
		   (= (y P1) (y P2))
		   (< (z P1) (z P2)))))))

(defun copy-point (P)
  (make-point (x P) (y P) (z P)))

(defun unique-points (Ps)
  "Takes list of points in sorted order & returns list with unique points."
  (cons
   (car Ps)
   (loop for (P1 . (P2 . rest)) on Ps
	 when (and P2
		   (not (P= P1 P2)))
	   collect P2)))

(defun P= (P1 P2)
  (and (= (x P1) (x P2))
       (= (y P1) (y P2))
       (= (z P1) (z P2))))

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

(defmethod print-object ((point point) stream)
  (format stream "~a" (list (x point) (y point) (z point))))

(defun make-point (x y z)
  (make-instance 'point :x x :y y :z z))

(defun make-translate (dx dy dz)
  (lambda (P)
    (make-point (+ dx (x P))
		(+ dy (y P))
		(+ dz (z P)))))

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
		   do (setf (aref table i j) (P-diff p1 p2))))
    table))

(defun P-diff (p1 p2)
  (list (- (x p1) (x p2)) (- (y p1) (y p2)) (- (z p1) (z p2))))

(defun printable-table (table)
  (loop for i below (car (array-dimensions table))
	collect (loop for j below (cadr (array-dimensions table))
		      collect (let ((P (aref table i j)))
				(list (x P) (y P) (z P))))))

