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
	  '((x y z) (x (- z) y) (x (- y) (- z)) (x z (- y)) ((- x) (- y) z)
	    ((- x) (- z) (- y)) ((- x) y (- z)) ((- x) z y) (y x (- z)) (y (- x) z)
	    (y z x) (y (- z) (- x)) ((- y) x z) ((- y) (- x) (- z))
	    ((- y) (- z) x) ((- y) z (- x)) (z x y) (z (- x) (- y))
	    (z (- y) x) (z y (- x)) ((- z) x (- y)) ((- z) (- x) y)
	    ((- z) y x) ((- z) (- y) (- x)))))

(defun solve (Ss)
  ;; Store each scanner as a list of all its beacons and its coordinates.
  (let ((final-map (list (list (pop Ss) (make-point 0 0 0)))))
    (loop while Ss
	  do (let* ((S (pop Ss)))
	       (multiple-value-bind (integrated-S S-coords)
		   (integrate-scanner S final-map)
		 (if integrated-S
		     (push (list integrated-S S-coords) final-map)
		     ;; Push it to the back of the list, integrate later.
		     (setf Ss (append Ss (list S)))))
	       (format t "scanners left: ~a~%" (length Ss))))
    (values (length (unique-points (points-sorted (apply #'nconc (mapcar #'first final-map)))))
	    (max-manhattan-distance (mapcar #'second final-map)))))

(defun max-manhattan-distance (points)
  (loop for P1 in points
	maximize (loop for P2 in points
		       maximize (manhattan-distance P1 P2))))

(defun manhattan-distance (P1 P2)
  (+ (abs (- (x P1) (x P2)))
     (abs (- (y P1) (y P2)))
     (abs (- (z P1) (z P2)))))

(defun integrate-scanner (S1 other-scanners)
  (let ((S1-rotations (all-rotations-of-scanner S1)))
    (block result
      (loop for (S2 S2-position) in other-scanners
	    do (multiple-value-bind (S1-rot offset)
		   (find-rotation-and-offset S2 S1-rotations)
		 (when S1-rot
		   (return-from result (values (loop for P in S1-rot collect (P+ P offset))
					       offset))))))))

(defun all-rotations-of-scanner (S)
  (loop for rotation in *all-rotations*
	collect (scanner-apply-function S rotation)))

(defun find-rotation-and-offset (S2 S1-rotations)
  (loop for S1-rot in S1-rotations
	for offset = (find-offset S2 S1-rot)
	when offset
	  return (values S1-rot offset)))

(defun find-offset(S2 S1)
  (let ((offset-counts (calculate-offset-counts S2 S1)))
    (loop for offset being each hash-key of offset-counts using (hash-value count)
	  when (>= count *common-beacons-threshold*)
	    return (destructuring-bind (x y z)
		       offset
		     (make-point x y z)))))

(defun calculate-offset-counts (S2 S1)
  (let ((offset-counts (make-hash-table :test 'equalp)))
    (loop for P2 in S2
	  do (loop for P1 in S1
		   do (let ((offset (P-offset P2 P1)))
			(setf (gethash offset offset-counts)
			      (1+ (or (gethash offset offset-counts) 0))))))
    offset-counts))

(defun P-offset (p1 p2)
  (list (- (x p1) (x p2)) (- (y p1) (y p2)) (- (z p1) (z p2))))

(defun P+ (P1 P2)
  (make-point (+ (x P1) (x P2))
	      (+ (y P1) (y P2))
	      (+ (z P1) (z P2))))

(defun P- (P1 P2)
  (make-point (- (x P1) (x P2))
	      (- (y P1) (y P2))
	      (- (z P1) (z P2))))

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
