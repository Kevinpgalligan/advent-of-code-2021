(defun read-navigation-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))

(defparameter *opening-braces* '(#\( #\[ #\{ #\<))
(defparameter *closing-braces* '(#\) #\] #\} #\>))
(defparameter *brace-pairs*
  (loop for o in *opening-braces*
	for c in *closing-braces*
	collect (cons o c)))

(defun opening-brace? (c)
  (member c *opening-braces*))

(defun matching-opening-brace (c)
  (car (rassoc c *brace-pairs* :test #'char=)))

(defun matching-closing-brace (c)
  (cdr (assoc c *brace-pairs* :test #'char=)))

(defparameter *error-score*
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(defun lookup-error-score (error-char)
  (cdr (assoc error-char *error-score*)))

(defparameter *completion-score*
  '((#\) 1)
    (#\] 2)
    (#\} 3)
    (#\> 4)))

(defun lookup-completion-score (char)
  (cadr (assoc char *completion-score*)))

(defun calculate-total-score (lines)
  (loop for line in lines
	sum (calculate-error-score line)))

(defun calculate-error-score (line)
  (block outer
    (let ((braces (list)))
      (loop for c across line
	    do (cond
		 ((opening-brace? c) (push c braces))
		 ((or (null braces)
		      (not (char= (car braces) (matching-opening-brace c))))
		  (return-from outer (lookup-error-score c)))
		 (t (pop braces)))))
    ;; There was no error!
    0))

(defun calculate-middle-score (lines)
  (let ((scores
	  (sort (mapcar #'calculate-completion-score
			(remove-if-not (lambda (line)
					 (zerop (calculate-error-score line)))
				       lines))
		#'<)))
    (nth (/ (1- (length scores)) 2) scores)))

(defun calculate-completion-score (line)
  (let ((braces (list))
	(score 0))
    (loop for c across line
	  ;; Assuming they're well-formed.
	  do (if (opening-brace? c)
		 (push c braces)
		 (pop braces)))
    (loop while braces
	  do (setf score (+ (lookup-completion-score (matching-closing-brace (pop braces)))
			    (* 5 score))))
    score))
