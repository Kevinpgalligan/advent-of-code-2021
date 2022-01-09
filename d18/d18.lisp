(defclass pair ()
  ((left
    :initarg :left
    :accessor left)
   (right
    :initarg :right
    :accessor right)
   (parent
    :initarg :parent
    :accessor parent)))

(defun pair (&key parent left right)
  (make-instance 'pair :parent parent :left left :right right))

(defun listify-tree (root-pair)
  (if (numberp root-pair)
      root-pair
      (list (listify-tree (left root-pair))
	    (listify-tree (right root-pair)))))

(defun parse-snailfish-number (s)
  (labels ((parse (i parent)
	     (if (not (char= #\[ (aref s i)))
		 (values (digit-char-p (aref s i)) (1+ i))
		 (let ((num (pair :parent parent)))
		   (multiple-value-bind (left new-i)
		       (parse (1+ i) num)
		     (setf (left num) left)
		     (setf i new-i))
		   (assert (char= #\, (aref s i)))
		   (multiple-value-bind (right new-i)
		       (parse (1+ i) num)
		     (setf (right num) right)
		     (setf i new-i))
		   (assert (char= #\] (aref s i)))
		   (values num (1+ i))))))
    (parse 0 nil)))

(defun sf-copy (sn &key parent)
  (if (numberp sn)
      sn
      (let ((copied (pair :parent parent)))
	(setf (left copied) (sf-copy (left sn) :parent copied))
	(setf (right copied) (sf-copy (right sn) :parent copied))
	copied)))

(defun read-numbers (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (snailfish-reduce! (parse-snailfish-number line)))))

(defun sum-snailfish-numbers (sns)
  (let (result)
    (loop for sn in sns
	  do (setf result (snailfish-reduce! (snailfish-add result sn))))
    (snailfish-magnitude result)))

(defun max-snailfish-sum (sns)
  (loop for (first . rest) on sns
	while rest
	maximize (loop for second in rest
		       maximize (max (snailfish-magnitude
				      (snailfish-reduce! (snailfish-add (sf-copy first) (sf-copy second))))
				     (snailfish-magnitude
				      (snailfish-reduce! (snailfish-add (sf-copy second) (sf-copy first))))))))

(defun snailfish-add (x y)
  (if (or (null x) (null y))
      (or x y)
      (let ((parent (pair)))
	(setf (left parent) x)
	(setf (right parent) y)
	(when (not (numberp x))
	  (setf (parent x) parent))
	(when (not (numberp y))
	  (setf (parent y) parent))
	parent)))

(defun snailfish-magnitude (n)
  (if (numberp n)
      n
      (+ (* 3 (snailfish-magnitude (left n)))
	 (* 2 (snailfish-magnitude (right n))))))

(defun snailfish-reduce! (sn)
  (loop while (snailfish-reduce-step! sn))
  sn)

(defun snailfish-reduce-step! (sn)
  (or (snailfish-explode-nested! sn)
      (snailfish-split-number! sn)))

(defun snailfish-explode-nested! (sn &key (depth 0))
  (and (not (numberp sn))
       (if (>= depth 4)
	   (let ((parent (parent sn)))
	     (inc-num-to-left sn)
	     (inc-num-to-right sn)
	     (if (equal (left parent) sn)
		 (setf (left parent) 0)
		 (setf (right parent) 0))
	     t)
	   (or (snailfish-explode-nested! (left sn) :depth (1+ depth))
	       (snailfish-explode-nested! (right sn) :depth (1+ depth))))))

(defmacro def-inc-adjacent (name ascend descend)
  `(defun ,name (current)
     (let ((delta (,ascend current)))
       ;; Keep ascending the tree until we reach the top
       ;; or find a parent on the appropriate side.
       (loop while (and (parent current)
			(equal current (,ascend (parent current))))
	     do (setf current (parent current)))
       (if (null (parent current))
	   ;; There's no adjacent number!
	   nil
	   (progn
	     (setf current (parent current))
	     (if (numberp (,ascend current))
		 ;; The adjacent number is a child of the node we ascended to.
		 (incf (,ascend current) delta)
		 ;; Descend until we find the adjacent number.
		 (progn
		   (setf current (,ascend current))
		   (loop while (not (numberp (,descend current)))
			 do (setf current (,descend current)))
		   (incf (,descend current) delta))))))))

(def-inc-adjacent inc-num-to-left left right)
(def-inc-adjacent inc-num-to-right right left)

(defun snailfish-split-number! (sn)
  (if (numberp sn)
      nil
      (or (snailfish-split-child! sn 'left)
	  (snailfish-split-child! sn 'right))))

(defun snailfish-split-child! (sn child-slot)
  (if (split? (slot-value sn child-slot))
      (setf (slot-value sn child-slot) (make-split-pair sn (slot-value sn child-slot)))
      (snailfish-split-number! (slot-value sn child-slot))))

(defun split? (sn)
  (and (numberp sn) (>= sn 10)))

(defun make-split-pair (parent n)
  (pair :parent parent :left (floor (/ n 2)) :right (ceiling (/ n 2))))
