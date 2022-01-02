(defun read-report (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defun calculate-power-consumption (readings)
  (let* ((bits (length (car readings)))
	 (counts (make-counts readings bits #\1))
	 (gamma-bits (make-string bits))
	 (n (length readings)))
    (loop for count across counts
	  for i = 0 then (1+ i)
	  do (setf (aref gamma-bits i) (if (>= (* 2 count) n) #\1 #\0)))
    (let* ((gamma (parse-integer gamma-bits :radix 2))
	   ;; Flip all the bits to get epsilon.
	   ;; Which is equivalent to XOR-ing with 2^bits-1.
	   (epsilon (logxor gamma (1- (expt 2 bits)))))
      (* gamma epsilon))))

(defun make-counts (readings bits digit-to-count)
  (let ((counts (make-array bits :initial-element 0)))
    (loop for reading in readings
	  do (loop for b across reading
		   for i = 0 then (1+ i)
		   when (char= digit-to-count b)
		     do (incf (aref counts i))))
    counts))

(defun calculate-life-support-rating (readings)
  (let* ((bits (length (car readings)))
	 (oxygen-rating (filter-readings (lambda (n-ones n-zeros)
					   (if (>= n-ones n-zeros)
					       'one
					       'zero))
					 readings
					 bits))
	 (co2-rating (filter-readings (lambda (n-ones n-zeros)
					(if (<= n-zeros n-ones)
					    'zero
					    'one))
				      readings
				      bits)))
    (format t "~a ~a~%" oxygen-rating co2-rating)
    (* oxygen-rating co2-rating)))

(defun filter-readings (filter-decision-fn readings bits)
  (loop while (not (null (cdr readings)))
	for i = 0 then (1+ i)
	do (let* ((one-counts (make-counts readings bits #\1))
		  (zero-counts (make-counts readings bits #\0))
		  (decision (funcall filter-decision-fn (aref one-counts i) (aref zero-counts i))))
	     (setf readings
		   (remove-if-not
		    (lambda (reading)
		      (or (and (eq decision 'one)
			       (char= (aref reading i) #\1))
			  (and (eq decision 'zero)
			       (char= (aref reading i) #\0))))
		    readings))))
  ;; Assuming there'll be 1 reading left!
  (parse-integer (car readings) :radix 2))
