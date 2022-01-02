(defun read-manual (path)
  (with-open-file (in path)
    (values
     (loop for line = (read-line in nil nil)
	   while (not (string= "" line))
	   collect (let ((comma-index (search "," line)))
		     (make-point (parse-integer (subseq line 0 comma-index))
				 (parse-integer (subseq line (1+ comma-index))))))
     (loop for line = (read-line in nil nil)
	   while line
	   collect (let ((equals-index (search "=" line)))
		     (make-fold (if (char= #\x (aref line (1- equals-index)))
				    'left
				    'up)
				(parse-integer (subseq line (1+ equals-index)))))))))

(defun make-fold (fold-type axis-coordinate)
  (list fold-type axis-coordinate))
(defun fold-type (fold) (car fold))
(defun fold-coordinate (fold) (cadr fold))

(defun make-point (x y) (list x y))
(defun point-x (p) (car p))
(defun point-y (p) (cadr p))

(defun count-dots-after-folds (points folds)
  (loop for fold in folds
	do (setf points (fold-paper points fold)))
  (length points))

(defun render-after-folds (points folds)
  (loop for fold in folds
	do (setf points (fold-paper points fold)))
  (let ((grid (make-array (loop for (x y) in points
				maximize (1+ x) into width
				maximize (1+ y) into height
				finally (return (list height width)))
			  :initial-element nil)))
    (loop for (x y) in points
	  do (setf (aref grid y x) t))
    (loop for i below (car (array-dimensions grid))
	  do (loop for j below (cadr (array-dimensions grid))
		   do (format t "~a" (if (aref grid i j) "#" ".")))
	  do (format t "~%"))))

(defun fold-paper (points fold)
  (unique-points (mapcar (lambda (p) (apply-fold fold p)) points)))

(defun unique-points (points)
  (loop for (p1 p2) on (cons nil (sort points #'point<))
	when (and p2 (not (equalp p2 p1)))
	  collect p2))

(defun point< (p1 p2)
  (or (< (point-x p1) (point-x p2))
      (and (= (point-x p1) (point-x p2))
	   (< (point-y p1) (point-y p2)))))

(defun apply-fold (fold pt)
  (case (fold-type fold)
    (up (make-point (point-x pt) (reflect-about (fold-coordinate fold) (point-y pt))))
    (left (make-point (reflect-about (fold-coordinate fold) (point-x pt)) (point-y pt)))
    (otherwise (error "invalid fold!"))))

(defun reflect-about (axis v)
  (if (< v axis)
      v
      (- (* 2 axis) v)))
