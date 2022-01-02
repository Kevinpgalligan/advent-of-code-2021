(defun read-template (path)
  (with-open-file (in path)
    (let ((initial-template (read-line in)))
      (read-line in)
      (values
       initial-template
       (loop for line = (read-line in nil nil)
	     while line
	     collect (parse-rule line))))))

(defun parse-rule (line)
  (destructuring-bind (from to)
      (split-string " -> " line)
    (make-rule from (char to 0))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun make-rule (pair output) (list pair output))
(defun rule-pair (rule) (car rule))
(defun rule-output (rule) (cadr rule))

(defmacro inc-count (key hash-table &optional amount)
  `(setf (gethash ,key ,hash-table)
	 (+ ,(or amount 1) (or (gethash ,key ,hash-table) 0))))

(defun make-count-table ()
  (make-hash-table :test 'equalp))

(defun solve (template rules steps)
  (let ((char-counts (make-count-table))
	(pair-counts (make-count-table)))
    (loop for c across template
	  do (inc-count c char-counts))
    (loop for i from 0 to (- (length template) 2)
	  do (let ((pair (subseq template i (+ 2 i))))
	       (inc-count pair pair-counts)))
    (loop repeat steps
	  do (let ((new-pair-counts (make-count-table)))
	       (loop for pair being each hash-key of pair-counts using (hash-value count)
		     for rule = (find-matching-rule pair rules)
		     when rule
		       do (inc-count (rule-output rule) char-counts count)
		       and do (inc-count (concatenate 'string (list (char pair 0) (rule-output rule)))
					 new-pair-counts
					 count)
		       and do (inc-count (concatenate 'string (list (rule-output rule) (char pair 1)))
					 new-pair-counts
					 count))
	       (setf pair-counts new-pair-counts)))
    (loop for count being each hash-value of char-counts
	maximizing count into highest-count
	minimizing count into lowest-count
	finally (return (- highest-count lowest-count)))))

(defun find-matching-rule (pair rules)
  (find-if (lambda (rule)
	     (equalp pair (rule-pair rule)))
	   rules))
