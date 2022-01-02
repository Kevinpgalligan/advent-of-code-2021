(declaim (optimize (debug 3)))

(defun read-risk-map (path &key tiles)
  (with-open-file (in path)
    (let* ((risk-map-as-lists
	     (loop for line = (read-line in nil nil)
		   while line
		   collect (loop for c across line
				 collect (digit-char-p c))))
	   (grid	
	     (make-array
	      (list (length risk-map-as-lists)
		    (length (car risk-map-as-lists)))
	      :initial-contents risk-map-as-lists)))
      (if tiles
	  (make-instance 'tiled-risk-map :grid grid :tiles tiles)
	  (make-instance 'plain-risk-map :grid grid)))))

(defun make-point (x y) (list x y))
(defun point-x (p) (car p))
(defun point-y (p) (cadr p))
(defun point= (p1 p2) (equalp p1 p2))
(defparameter *start-point* (make-point 0 0))

(defun adjacent-points (risk-map point)
  (remove-if-not
   (lambda (adj)
     (and (<= 0 (point-x adj) (point-x (final-point risk-map)))
	  (<= 0 (point-y adj) (point-y (final-point risk-map)))))
   (loop for (dx dy) in '((-1 0) (+1 0) (0 -1) (0 +1))
	 collect (make-point (+ dx (point-x point))
			     (+ dy (point-y point))))))

(defclass plain-risk-map ()
  ((grid
    :initarg :grid)))

(defclass tiled-risk-map ()
  ((grid
    :initarg :grid)
   (tiles
    :initarg :tiles)))

(defun grid-width (risk-map)
  (with-slots (grid) risk-map
    (cadr (array-dimensions grid))))

(defun grid-height (risk-map)
  (with-slots (grid) risk-map
    (car (array-dimensions grid))))

(defgeneric risk (risk-map point))
(defgeneric final-point (risk-map))

(defmethod risk ((risk-map plain-risk-map) point)
  (with-slots (grid) risk-map
    (aref grid (point-y point) (point-x point))))

(defmethod final-point ((risk-map plain-risk-map))
  (make-point (1- (grid-width risk-map)) (1- (grid-height risk-map))))

(defmethod risk ((risk-map tiled-risk-map) point)
  (with-slots (grid) risk-map
    (let ((original-risk (aref grid
			       (mod (point-y point) (grid-height risk-map))
			       (mod (point-x point) (grid-width risk-map))))
	  (offset (+ (truncate (point-x point) (grid-width risk-map))
		     (truncate (point-y point) (grid-height risk-map)))))
      ;; This is a bit awkward because it has to wrap
      ;; back around to 1.
      ;; Subtract 1 from 9, gives 8. Add offset of 1, gives 9.
      ;; Mod by 9, gives 0. Add back 1, gives 1.
      ;; So we wrap from 9 to 1!
      ;; Similarly: 8-1=7, 7+1=8, 8%9=8, 8+1=9.
      ;; And 1-1=0, 0+1=1, 1%8=1, 1+1=2.
      (1+ (mod (+ (1- original-risk) offset) 9)))))

(defmethod final-point ((risk-map tiled-risk-map))
  (with-slots (tiles) risk-map
    (make-point
     (1- (* tiles (grid-width risk-map)))
     (1- (* tiles (grid-height risk-map))))))

;; REMEMBER: the risk of the starting point
;; isn't counted, so you should subtract it before inputting
;; your answer on AoC.
(defun lowest-risk-path (risk-map)
  (let* ((initial-point (final-point risk-map))
	 (partial-paths (list (make-partial-path (list initial-point)
						 (risk risk-map initial-point))))
	 (visited (make-hash-table :test 'equalp)))
    (loop for path-to-explore = (pop partial-paths)
	  for point = (most-recent-point path-to-explore)
	  when (point= *start-point* point)
	    return path-to-explore
	  when (not (gethash point visited))
	    do (loop for next-point in (adjacent-points risk-map point)
		     do (insert-path! partial-paths
				      (extended-path risk-map path-to-explore next-point)))
	    and do (setf (gethash point visited) t))))

(defun make-partial-path (points total-risk)
  (list points total-risk))
(defun pp-points (pp) (car pp))
(defun pp-risk (pp) (cadr pp))
(defun most-recent-point (pp)
  (car (pp-points pp)))
(defun extended-path (risk-map path point)
  (make-partial-path (cons point (pp-points path))
		     (+ (pp-risk path) (risk risk-map point))))

(defmacro insert-path! (place path)
  (let ((path-evaluated (gensym)))
    `(let ((,path-evaluated ,path))
       (if (or (null ,place) (< (pp-risk ,path-evaluated) (pp-risk (car ,place))))
	   (push ,path-evaluated ,place)
	   (loop for rest on ,place
		 when (or (null (cdr rest))
			  (< (pp-risk ,path-evaluated) (pp-risk (cadr rest))))
		   do (progn
			(let ((insert-cons (cons ,path-evaluated (cdr rest))))
			  (setf (cdr rest) insert-cons))
			(return)))))))

(defun dump-risk-map (rm path out)
  "HTML visualisation of the path through the risk map.
RM should be a risk map.
PATH should be a path through the risk map, not a file path.
OUT should be a writeable file stream."
  (loop for y upto (point-y (final-point rm))
	do (loop for x upto (point-x (final-point rm))
		 do (let ((pt (make-point x y)))
		      (when (member pt (pp-points path) :test #'point=)
			(format out "<b>"))
		      (format out "~a," (risk rm pt))
		      (when (member pt (pp-points path) :test #'point=)
			(format out "</b>"))))
	do (format out "<br>")))


