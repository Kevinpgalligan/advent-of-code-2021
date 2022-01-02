;; How to use:
;;CL-USER> (multiple-value-bind (draws boards)
;;	     (read-bingo-input "/path/to/input")
;;	   (calculate-winner-score draws boards))
;;39902

(defparameter *bingo-board-side-length* 5)
(defparameter *bingo-board-dimensions*
  (list *bingo-board-side-length*
	*bingo-board-side-length*))

(defun read-bingo-input (path)
  (with-open-file (in path)
    (values
     (mapcar #'parse-integer
	     (split-string "," (read-line in)))
     (loop for board = (read-bingo-board in)
	   while board
	   collect board))))

(defun calculate-winner-score (draws boards)
  (block outer
    (loop for draw in draws
	  do (loop for board in boards
		   when (and (mark-board! board draw)
			     (board-winner? board))
		     do (return-from outer (calculate-score draw board))))))

(defun calculate-score (last-draw board)
  (* last-draw (reduce #'+ (unmarked-numbers board))))
  
(defun calculate-last-winner-score (draws boards)
  (loop for draw in draws
	do (loop for board in boards
		 do (mark-board! board draw))
	when (and (null (cdr boards))
		  (board-winner? (car boards)))
	  return (calculate-score draw (car boards))
	do (setf boards (remove-if #'board-winner? boards))))

(defclass bingo-board ()
  ((numbers
    :initarg :numbers)
   (marks
    :initarg :marks)))

(defun unmarked-numbers (board)
  (with-slots (marks numbers)
      board
    (apply #'nconc
	   (loop for i below *bingo-board-side-length*
		 collect (loop for j below *bingo-board-side-length*
			       when (not (aref marks i j))
				 collect (aref numbers i j))))))

(defun mark-board! (board number)
  (block outer
    (with-slots (numbers marks)
	board
      (loop for i below *bingo-board-side-length*
	    do (loop for j below *bingo-board-side-length*
		     when (= (aref numbers i j) number)
		       do (progn
			    (setf (aref marks i j) t)
			    (return-from outer t)))))))

(defun board-winner? (board)
  (with-slots (marks)
      board
    (loop for slice-marked? in (list #'row-marked? #'col-marked?)
	  thereis (loop for i below *bingo-board-side-length*
		    thereis (funcall slice-marked? marks i)))))

(defun row-marked? (marks i)
  (loop for j below *bingo-board-side-length*
	always (aref marks i j)))

(defun col-marked? (marks j)
  (loop for i below *bingo-board-side-length*
	always (aref marks i j)))

(defun read-bingo-board (in)
  (let ((empty-line (read-line in nil nil)))
    (if (null empty-line)
	nil
	(make-instance
	 'bingo-board
	 :numbers (make-array *bingo-board-dimensions*
			      :initial-contents
			      (loop repeat *bingo-board-side-length*
				    collect (mapcar #'parse-integer
						    (remove-if (lambda (s) (zerop (length s)))
							       (split-string " " (read-line in))))))
	 :marks (make-array *bingo-board-dimensions* :initial-element nil)))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))
