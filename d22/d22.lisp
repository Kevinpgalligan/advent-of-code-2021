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

(defclass cube-union ()
  ((c1
    :initarg :c1
    :reader c1)
   (c2
    :initarg :c2
    :reader c2)))

(defclass cube-complement ()
  ((c1
    :initarg :c1
    :reader c1)
   (c2
    :initarg :c2
    :reader c2)))

(defun empty-cube ()
  (make-instance 'cube
		 :xrange *empty-range*
		 :yrange *empty-range*
		 :zrange *empty-range*))

(defgeneric volume (A))
(defgeneric cube-intersection (A B))
(defgeneric cube-empty-p (A))

(defmethod cube-empty-p ((cube cube))
  (zerop (volume cube)))

(defmethod cube-empty-p ((U cube-union))
  nil)

(defmethod cube-empty-p ((C cube-complement))
  nil)

(defmethod volume ((cube cube))
  (* (range-size (xrange cube))
     (range-size (yrange cube))
     (range-size (zrange cube))))

(defmethod volume ((U cube-union))
  (+ (volume (c1 U))
     (volume (c2 U))
     (- (volume (cube-intersection (c1 U) (c2 U))))))

(defmethod volume ((C cube-complement))
  (- (volume (c1 C))
     (volume (cube-intersection (c1 C) (c2 C)))))

(defmethod cube-intersection ((U cube-union) (cube cube))
  (cube-union (cube-intersection (c1 U) cube)
	      (cube-intersection (c2 U) cube)))

(defmethod cube-intersection ((C cube-complement) (cube cube))
  (cube-complement (cube-intersection (c1 C) cube)
		   (c2 C)))

(defmethod cube-intersection ((c1 cube) (c2 cube))
  (make-instance 'cube
		 :xrange (range-intersect (xrange c1) (xrange c2))
		 :yrange (range-intersect (yrange c1) (yrange c2))
		 :zrange (range-intersect (zrange c1) (zrange c2))))

(defun cube-union (c1 c2)
  (cond
    ((cube-empty-p c1) c2)
    ((cube-empty-p c2) c1)
    (t (make-instance 'cube-union :c1 c1 :c2 c2))))

;; TODO
(defun cube-complement (c1 c2)
  (if (cube-contains c1 (cube-intersection c1 c2))
      (empty-cube)
      (make-instance 'cube-complement :c1 c1 :c2 c2)))

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
  (let ((on-region (empty-cube)))
    (loop for (on-off cube) in cubes
	  do (setf on-region (if (eq on-off 'on)
				 (cube-union on-region cube)
				 (cube-complement on-region cube))))
    (volume (if restricted?
		(cube-intersection
		 on-region
		 (make-instance 'cube
				:xrange (range -50 50)
				:yrange (range -50 50)
				:zrange (range -50 50)))
		on-region))))
