;; dependencies: cl-ppcre
;; (ql:quickload 'cl-ppcre)

;; |A U B| = |A| + |B| - |A n B|
;; |A - B| = |A| - |A n B|
;; (A U B) n C = (A n C) U (B n C)
;; (A - B) n C = (A n C) - B
;; ((A U B) - C) n D = ((A U B) n D) - C = ((A n D) U (B n D)) - C

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

(defclass union ()
  ((c1
    :initarg :c1
    :reader c1)
   (c2
    :initarg :c2
    :reader c2)))

(defclass complement ()
  ((c1
    :initarg :c1
    :reader c1)
   (c2
    :initarg :c2
    :reader c2)))

(defgeneric volume (c))

(defmethod volume ((union union))
  (+ (cube-volume (c1 union))
     (cube-volume (c2 union))
     (- (cube-volume (intersection (c1 union) (c2 union))))))

(defmethod volume ((complement complement))
  (- (cube-volume (c1 complement))
     (cube-volume (cube-intersection (c1 complement) (c2 complement)))))

(defun union (c1 c2)
  (cond
    ((cube-empty-p c1) c2)
    ((cube-empty-p c2) c1)
    (t (list 'union c1 c2))))

(defun cube-complement (c1 c2)
  (if (cube-empty-p (cube-intersect c1 c2))))

(defun cube-intersect (c1 c2)
  (let ((nxr (range-intersect (xrange c1) (xrange c2)))
	(nyr (range-intersect (yrange c1) (yrange c2)))
	(nzr (range-intersect (zrange c1) (zrange c2))))
    (if (loop for r in (list nxr nyr nzr)
		thereis (range-empty-p r))
	*empty-cube*
	(make-instance 'cube :xrange nxr :yrange nyr :zrange nzr))))

(defun cube-empty-p (cube)
  (or (range-empty-p (xrange cube))
      (range-empty-p (yrange cube))
      (range-empty-p (zrange cube))))

(defun cube-volume (cube)
  (* (range-size (xrange cube))
     (range-size (yrange cube))
     (range-size (zrange cube))))

(defun cube-contains-p (c1 c2)
  (cond
    ((cube-empty-p c1) nil)
    ((cube-empty-p c2) t)
    (t
     (loop for r in '(xrange yrange zrange)
	   always (range-contains (slot-value c1 r) (slot-value c2 r))))))

(defmethod print-object ((cube cube) stream)
  (format stream "{x=~a; y=~a; z=~a}" (xrange cube) (yrange cube) (zrange cube)))

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
  (1+ (- (range-high range) (range-low range))))

(defun range-low (range) (car range))
(defun range-high (range) (cadr range))

(defun range-empty-p (range)
  (eq *empty-range* range))

(defun range-intersect (r1 r2)
  (if (or (< (range-high r1) (range-low r2))
	  (< (range-high r2) (range-low r1)))
      *empty-range*
      (range (max (range-low r1) (range-low r2))
	     (min (range-high r1) (range-high r2)))))

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
  (let (on-cubes)
    (loop for (on-off cube) in cubes
	  do (if (eq 'on on-off)
		 (let ((cubes-to-add (list cube)))
		   (loop for cube in on-cubes
			 do (setf cubes-to-add
				  (nconc
				   (mapcar (lambda (cube-to-add)
					     (cube-subtract cube-to-add cube))
					   cubes-to-add)))))))
    (loop for cube in on-cubes
	  sum (cube-volume (if restricted?
			       (cube-intersect cube
					       (make-instance 'cube
							      :xrange (range -50 50)
							      :yrange (range -50 50)
							      :zrange (range -50 50)))
			       cube)))))
