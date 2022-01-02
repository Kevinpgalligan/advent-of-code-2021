(defparameter *empty-range* 'empty-range)
(defparameter *negative-infinity* '-inf)
(defparameter *infinity* 'inf)

(defun make-disjoint-range (r1 r2)
  (cond
    ((range-empty-p r1) r2)
    ((range-empty-p r2) r1)
    (t (list 'disjoint r1 r2))))

(defun disjoint-first (x) (second x))
(defun disjoint-second (x) (third x))
(defun disjoint-p (x) (and (listp x)
			   (eq 'disjoint (car x))))

(defun rangebound<= (x y)
  (rangebound-cmp x y #'<=))

(defun rangebound< (x y)
  (rangebound-cmp x y #'<))

(defun rangebound-cmp (x y less-than-operator)
  (cond
    ((eq x *negative-infinity*) t)
    ((eq y *infinity*) t)
    ((eq x *infinity*) (eq y *infinity*))
    ((eq y *negative-infinity*) (eq x *negative-infinity*))
    (t (funcall less-than-operator x y))))

(defun rangebound-max (x y) (if (rangebound<= x y) y x))
(defun rangebound-min (x y) (if (rangebound<= x y) x y))

(defun make-range (lo hi)
  (assert (not (eq *negative-infinity* hi)))
  (assert (not (eq *infinity* lo)))
  (assert (rangebound<= lo hi))
  (list lo hi))

(defun range-lo (r) (car r))
(defun range-hi (r) (cadr r))

(defun range-empty-p (r)
  (eq r *empty-range*))

(defun range-overlap (r1 r2)
  (cond
    ((disjoint-p r1)
     (make-disjoint-range
      (range-overlap (disjoint-first r1) r2)
      (range-overlap (disjoint-second r1) r2)))
    ((disjoint-p r2)
     (make-disjoint-range
      (range-overlap r1 (disjoint-first r2))
      (range-overlap r1 (disjoint-second r2))))
    ((or (range-empty-p r1)
	 (range-empty-p r2)
	 (rangebound< (range-hi r1) (range-lo r2))
	 (rangebound< (range-hi r2) (range-lo r1)))
     *empty-range*)
    (t
     (make-range (rangebound-max (range-lo r1) (range-lo r2))
		 (rangebound-min (range-hi r1) (range-hi r2))))))

(defun ranges-overlap-p (r1 r2)
  (not (range-empty-p (range-overlap r1 r2))))

(defun range-spans-integer-p (r)
  (if (disjoint-p r)
      (or (range-spans-integer-p (disjoint-first r))
	  (range-spans-integer-p (disjoint-second r)))
      (and
       (not (range-empty-p r))
       (not (< (floor (range-hi r)) (ceiling (range-lo r)))))))

(defun solve (lx ux ly uy)
  (apply
   #'nconc
   (loop for vx0 from 1 to ux
	 collect (count-y-velocities-on-target vx0 lx ux ly uy))))

(defun calculate-t-solution-range (v0 l u &key drag?)
  ;; Given initial velocity that decreases by 1 each timestep, when
  ;; will the projectile fall within the range [l,u]?
  ;; If there's drag
  (let ((range-without-drag
	  (range-overlap
	   (solve-quadratic-inequality -1/2 (+ 1/2 v0) (- u) #'<=)
	   (solve-quadratic-inequality -1/2 (+ 1/2 v0) (- l) #'>=))))
    (if drag?
	;; Two considerations for drag.
	;; First, it can't go past the target and come back, which
	;; would be a disjoint range. So only use the first part of
	;; a disjoint range.
	;; Second, if it stops in the target range, it'll stay there
	;; for eternity.
	;; Jeez, this apply-if function is UGLY.
	(apply-if
	 (lambda (r)
	   ;; To infinity and beyond!
	   (make-range (range-lo r) *infinity*))
	 (lambda (r)
	   (declare (ignore r))
	   ;; Does it halt in the target range? #quickmathz
	   (<= l (- (square v0) (/ (* v0 (1- v0)) 2)) u))
	 (apply-if #'disjoint-first #'disjoint-p range-without-drag))
	range-without-drag)))

(defun apply-if (f predicate x)
  (if (funcall predicate x)
      (funcall f x)
      x))

(defun solve-quadratic-inequality (a b c cmp-fn)
  (let ((sqrt-part (+ (* b b) (- (* 4 a c)))))
    (if (< sqrt-part 0)
	;; There are no roots to the equality! So either
	;; the inequality is ALWAYS true, or it's NEVER true.
	;; Doesn't matter where we evaluate the quadratic.
	(if (funcall cmp-fn (evaluate-quadratic a b c 0) 0)
	    (make-range *negative-infinity* *infinity*)
	    *empty-range*)
	(let* ((root- (/ (+ (- b) (intsqrt sqrt-part)) (* 2 a)))
	       (root+ (/ (- (- b) (intsqrt sqrt-part)) (* 2 a)))
	       (x (/ (+ root+ root-) 2)))
	  ;; Evaluate the equation between the two roots.
	  ;; If the inequality holds there (e.g. <= 0), then
	  ;; the range should be between the roots. Otherwise,
	  ;; it should be on either side of the roots.
	  (if (funcall cmp-fn (evaluate-quadratic a b c x) 0)
	      (make-range root- root+)
	      (make-disjoint-range
	       (make-range *negative-infinity* root-)
	       (make-range root+ *infinity*)))))))

(defun evaluate-quadratic (a b c x)
  (+ (* a x x) (* b x) c))

(defun intsqrt (x)
  "If it's possible to calculate the exact square root of a number, does so."
  (if (and (rationalp x) (not (= 1 (denominator x))))
      (/ (intsqrt (numerator x)) (intsqrt (denominator x)))
      (let ((float-sqrt (sqrt x)))
	(or (and (= x (square (floor float-sqrt)))
		 (floor float-sqrt))
	    (and (= x (square (ceiling float-sqrt)))
		 (ceiling float-sqrt))
	    float-sqrt))))

(defun square (x)
  (* x x))

(defun count-y-velocities-on-target (vx0 lx ux ly uy)
  (let ((t-range-for-x (calculate-t-solution-range vx0 lx ux :drag? t)))
    (loop for vy0 = ly then (1+ vy0)
	  while (<= vy0 (1- (- ly)))
	  for t-range = (range-overlap t-range-for-x
				       (calculate-t-solution-range vy0 ly uy)) 
	  when (range-spans-integer-p t-range)
	    collect (list vx0 vy0 t-range))))

(defun project-position (v0 steps &key drag?)
  (let ((x 0)
	(v v0))
    (cons 0
	  (loop repeat steps
		do (incf x v)
		collect x
		do (when (not (and drag? (zerop v)))
		     (decf v))))))
