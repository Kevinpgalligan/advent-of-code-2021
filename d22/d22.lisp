;; dependencies: cl-ppcre
;; (ql:quickload 'cl-ppcre)

;; |A U B| = |A| + |B| - |A n B|
;; |A - B| = |A| - |A n B|
;; (A U B) n C = (A n C) U (B n C)
;; (A - B) n C = (A n C) - B
;; ((A U B) - C) n D = ((A U B) n D) - C = ((A n D) U (B n D)) - C

(defparameter *empty-range* 'empty-range)

(defun range (l h)
  (if (> l h)
      *empty-range*
      (list l h)))

(defun range= (r1 r2)
  (if (or (range-empty-p r1) (range-empty-p r2))
      (and (range-empty-p r1) (range-empty-p r2))
      (and (= (range-low r1) (range-low r1))
	   (= (range-high r1) (range-high r2)))))

(defun range-contains (r1 r2)
  "Does range r1 contain r2?"
  (range= r2 (range-intersect r1 r2)))

(defun range-size (range)
  (if (eq *empty-range* range)
      0
      (1+ (- (range-high range) (range-low range)))))

(defun range-low (range) (car range))
(defun range-high (range) (cadr range))

(defun range-empty-p (range)
  (eq *empty-range* range))

(defun range-intersect (r1 r2)
  (if (or (range-empty-p r1)
	  (range-empty-p r2)
	  (< (range-high r1) (range-low r2))
	  (< (range-high r2) (range-low r1)))
      *empty-range*
      (range (max (range-low r1) (range-low r2))
	     (min (range-high r1) (range-high r2)))))

(defclass cube ()
  ((xrange
    :initarg :xrange
    :reader xrange)
   (yrange
    :initarg :yrange
    :reader yrange)
   (zrange
    :initarg :zrange
    :reader zrange)))

(defun volume (cube)
  (* (range-size (xrange cube))
     (range-size (yrange cube))
     (range-size (zrange cube))))

(defun cube-intersection (c1 c2)
  (make-instance 'cube
		 :xrange (range-intersect (xrange c1) (xrange c2))
		 :yrange (range-intersect (yrange c1) (yrange c2))
		 :zrange (range-intersect (zrange c1) (zrange c2))))

(defun cube-empty-p (cube)
  (zerop (volume cube)))

(defmethod print-object ((cube cube) stream)
  (format stream "{x=~a, y=~a, z=~a}" (xrange cube) (yrange cube) (zrange cube)))

(defun load-cubes (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (list
		   (if (string= "off" (subseq line 0 3)) 'off 'on)
		   (let (ranges)
		     (cl-ppcre:do-register-groups (l h)
		       ("[xyz]=(-?\\d+)\\.\\.(-?\\d+)" line)
		       (push (range (parse-integer l) (parse-integer h)) ranges))
		     (destructuring-bind (xr yr zr)
			 ranges
		       (make-instance 'cube :xrange xr :yrange yr :zrange zr)))))))

(defun solve (cubes &key restricted?)
  (let (add-cubes subtract-cubes)
    (loop for (on-off cube) in cubes
	  do (let (new-add-cubes new-sub-cubes)
	       (loop for ac in add-cubes
		     do (let ((in (cube-intersection cube ac)))
			  (when (not (cube-empty-p in))
			    (push in new-sub-cubes))))
	       (loop for sc in subtract-cubes
		     do (let ((in (cube-intersection cube sc)))
			  (when (not (cube-empty-p in))
			    (push in new-add-cubes))))
	       (setf add-cubes (nconc new-add-cubes add-cubes))
	       (setf subtract-cubes (nconc new-sub-cubes subtract-cubes))
	       (when (eq on-off 'on)
		 (push cube add-cubes))))
    (let ((small-range-cube
	    (make-instance 'cube
			   :xrange (range -50 50)
			   :yrange (range -50 50)
			   :zrange (range -50 50))))
      (- (loop for cube in add-cubes
	       sum (volume (if restricted?
			       (cube-intersection cube small-range-cube)
			       cube)))
	 (loop for cube in subtract-cubes
	       sum (volume (if restricted?
			       (cube-intersection cube small-range-cube)
			       cube)))))))
