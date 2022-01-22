(defun solve-deterministic (p1 p2)
  (let ((score1 0)
	(score2 0)
	(turn 'p1)
	(dice 1)
	(num-rolls 0))
    (loop while (and (< score1 1000)
		     (< score2 1000))
	  do (let ((dx (mod (* 3 (1+ dice)) 10)))
	       (setf dice (mod (+ 3 dice) 10))
	       (incf num-rolls 3)
	       (if (eq turn 'p1)
		   (progn
		     (setf p1 (getpos (+ dx p1)))
		     (incf score1 p1)
		     (setf turn 'p2))
		   (progn
		     (setf p2 (getpos (+ dx p2)))
		     (incf score2 p2)
		     (setf turn 'p1))))
	  finally (return (values num-rolls score1 score2)))))

(defun solve-dirac (start-pos1 start-pos2)
  (let ((seen (make-hash-table :test 'equalp))
	;; 111, 112, 113, 121, 122, 123, 131, 132,
	;; 133, 211, 212, 213, 221, 222, 223, 231,
	;; 232, 233, 311, 312, 313, 321, 322, 323,
	;; 331, 332, 333
	(universes '(3 4 5 4 5 6 5 6 7 4 5 6 5 6 7 6 7 8 5 6 7 6 7 8 7 8 9)))
    (labels ((solve (pos1 pos2 score1 score2 turn)
	       (let ((state (list pos1 pos2 score1 score2 turn)))
		 (cond
		   ((>= score1 21) (values 1 0))
		   ((>= score2 21) (values 0 1))
		   ((gethash state seen)
		    (destructuring-bind (wins1 wins2)
			(gethash state seen)
		      (values wins1 wins2)))
		   (t
		    (let ((wins1 0) (wins2 0))
		      (if (eq turn 'p1)
			  ;; Yay, duplication.
			  (loop for k in universes
				do (multiple-value-bind (d-wins1 d-wins2)
				       (let ((new-pos1 (getpos (+ pos1 k))))
					 (solve new-pos1 pos2 (+ score1 new-pos1) score2 'p2))
				     (incf wins1 d-wins1)
				     (incf wins2 d-wins2)))
			  (loop for k in universes
				do (multiple-value-bind (d-wins1 d-wins2)
				       (let ((new-pos2 (getpos (+ pos2 k))))
					 (solve pos1 new-pos2 score1 (+ score2 new-pos2) 'p1))
				     (incf wins1 d-wins1)
				     (incf wins2 d-wins2))))
		      (setf (gethash state seen) (list wins1 wins2))
		      (values wins1 wins2)))))))
    (solve start-pos1 start-pos2 0 0 'p1))))

(defun getpos (x)
  (if (> x 10)
      (1+ (mod x 11))
      x))
