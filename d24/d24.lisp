;; dependency:
;;   (ql:quickload 'trivia)

;; Other approaches that sound cool:
;;   - compile the MONAD program on the fly so that it
;;     runs really fast, use that to do brute-force.
;;   - generate constraints and pass them to a constraint solver.

(defun load-program (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (mapcar #'parse-token (split-string " " line)))))

(defun parse-token (token)
  (if (or (digit-char-p (aref token 0))
	  (char= (aref token 0) #\-))
      (parse-integer token)
      (intern (string-upcase token))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun execute-program (instructions inputs)
  (let ((w 0) (x 0) (y 0) (z 0))
    ;; So that the registers are visible to the functions.
    (declare (special w x y z))
    (labels ((gv (a) (if (numberp a) a (symbol-value a))))
      (loop for instruction in instructions
	    do (trivia:match instruction
		 ((list 'inp a)
		  (when (null inputs)
		    (error "unavailable input!"))
		  (setf (symbol-value a) (pop inputs)))
		 ((list 'add a b)
		  (incf (symbol-value a) (gv b)))
		 ((list 'mul a b)
		  (setf (symbol-value a) (* (gv a) (gv b))))
		 ((list 'div a b)
		  (setf (symbol-value a)
			(* (sign (gv a)) (sign (gv b)) (floor (abs (gv a)) (abs (gv b))))))
		 ((list 'mod a b)
		  (setf (symbol-value a) (mod (gv a) (gv b))))
		 ((list 'eql a b)
		  (setf (symbol-value a) (if (= (gv a) (gv b)) 1 0)))))
      (list w x y z))))

(defun sign (n)
  (if (>= n 0) 1 0))

(defun num-to-monad-input (n)
  (loop for c across (write-to-string n)
	collect (digit-char-p c)))

(defun monad-search (monad-program &key smallest)
  ;; See the 'notes' file for an explanation of why
  ;; this algorithm works.
  (let ((offsets (loop for instruction in monad-program
		       when (and (eq 'add (first instruction))
				 (eq 'x (second instruction))
				 (numberp (third instruction)))
			 collect (third instruction)))
	(divides (loop for instruction in monad-program
		       when (and (eq 'div (first instruction))
				 (eq 'z (second instruction)))
			 collect (third instruction)))
	(adds (loop for instruction in monad-program
		    for i = 0 then (1+ i)
		    when (and (eq 'add (first instruction))
			      (eq 'y (second instruction))
			      (numberp (third instruction))
			      (not (member (mod i 18) '(9 11))))
		      collect (third instruction)))
	(search-order (funcall (if smallest #'identity #'reverse)
			       (loop for d from 1 to 9 collect d))))
    (labels ((recurse (z digits offsets divides adds)
	       (if (null offsets)
		   (and (zerop z) (reverse digits))
		   (block search-block
		     (loop for d in search-order
			   do (block attempt
				(let ((new-z (floor z (car divides))))
				  (when (not (= d (+ (car offsets) (mod z 26))))
				    (setf new-z (+ (* new-z 26) d (car adds))))
				  (when (>= new-z (product divides))
				    ;; z can't possibly be 0 at the end
				    (return-from attempt))
				  (let ((result (recurse new-z
							 (cons d digits)
							 (cdr offsets)
							 (cdr divides)
							 (cdr adds))))
				    (when result
				      (return-from search-block result))))))))))
      (recurse 0 nil offsets divides adds))))

(defun product (nums)
  (let ((p 1))
    (loop for n in nums
	  do (setf p (* p n)))
    p))

(defun passes-monad (monad-program n)
  (let ((monad-input (num-to-monad-input n)))
    (and (not (member 0 monad-input :test '=))
	 (destructuring-bind (w x y z)
	     (execute-program monad-program monad-input)
	   (declare (ignore w x y))
	   (zerop z)))))

(defun derive-register-values (instructions)
  (let ((w 0) (x 0) (y 0) (z 0) (k 1))
    (declare (special w x y z k))
    (labels ((gv (a) (if (or (null a) (numberp a)) a (symbol-value a))))
      (loop for instruction in instructions
	    do (let* ((name (car instruction))
		      (a (cadr instruction))
		      (b (caddr instruction))
		      (va (gv a))
		      (vb (gv b))
		      (both-nums (and (numberp va) (numberp vb))))
		 (case name
		   (inp
		    (setf (symbol-value a)
			  (intern (string-upcase (concatenate 'string "n" (write-to-string k)))))
		    (incf k))
		   (add
		    (setf (symbol-value a)
			  (cond
			    ((eq 0 va) vb)
			    ((eq 0 vb) va)
			    (both-nums (+ va vb))
			    (t `(+ ,va ,vb)))))
		   (mul
		    (setf (symbol-value a)
			  (cond
			    ((or (eq 0 va) (eq 0 vb)) 0)
			    ((eq 1 va) vb)
			    ((eq 1 vb) va)
			    (both-nums (* va vb))
			    (t `(* ,va ,vb)))))
		   (div
		    (setf (symbol-value a)
			  (cond
			    ((eq 1 vb) va)
			    ((eq 0 va) 0)
			    (both-nums (floor va vb))
			    (t `(/ ,va ,vb)))))
		   (mod
		    (setf (symbol-value a)
			  (cond
			    ((eq 1 vb) 0)
			    ((eq 0 va) 0)
			    (both-nums (mod va vb))
			    (t `(% ,va ,vb)))))
		   (eql
		    (setf (symbol-value a)
			  (cond
			    ;; Inversions & redundant ifs.
			    ((and (member vb '(0 1))
				  (listp va)
				  (eq (car va) 'if))
			     (if (eq vb (caddr va))
				 va
				 `(if ,(cadr va) ,vb ,(mod (1+ vb) 2))))
			    ((and (member va '(0 1))
				  (listp vb)
				  (eq (car vb) 'if))
			     (if (eq va (caddr vb))
				 vb
				 `(if ,(cadr vb) ,va ,(mod (1+ va) 2))))
			    ((and (numberp va) (numberp vb))
			     (if (= va vb) 1 0))
			    ((or (and (numberp va) (symbolp vb))
				 (and (symbolp va) (numberp vb)))
			     (if (not (<= 1 (if (numberp va) va vb) 9))
				 0
				 1))
			    (t
			     `(if (= ,va ,vb) 1 0))))))))
      (list w x y z k))))
