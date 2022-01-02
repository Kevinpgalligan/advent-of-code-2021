(defun read-entries (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (destructuring-bind (raw-input-signals raw-output-signals)
		      (split-string " | " line)
		    (make-entry (split-string " " raw-input-signals)
				(split-string " " raw-output-signals))))))

(defun make-entry (input-signals output-signals) (list input-signals output-signals))
(defun entry-inputs (entry) (car entry))
(defun entry-outputs (entry) (cadr entry))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defparameter *digit-specs*
  '((0 "abcefg")
    (1 "cf")
    (2 "acdeg")
    (3 "acdfg")
    (4 "bcdf")
    (5 "abdfg")
    (6 "abdefg")
    (7 "acf")
    (8 "abcdefg")
    (9 "abcdfg")))

(defparameter *segments*
  (loop for x across "abcdefg"
	collect x))

(defun permutations (list)
  (if (null list)
      (list (list))
      (mapcan (lambda (partial-permutation)
		(all-inserts (car list) partial-permutation))
	      (permutations (cdr list)))))

(defun all-inserts (x list)
  (cons
   (nreverse
    (cons x
	  (reverse list)))
   (let ((previous (list)))
     (loop for remaining on list
	   ;; Have to be careful not to destroy anything before we're
	   ;; done using it.
	   collect (nconc (reverse previous)
			  (list x)
			  (copy-list remaining))
	   do (push (car remaining) previous)))))

;; So we don't have to recompute it all the time.
;; There should only be 5000 or so.
(defparameter *decode-maps*
  (loop for permutation in (permutations *segments*)
	collect (loop for x in *segments*
		      for y in permutation
		      collect (cons x y))))

;; Part 1.
(defun count-easy-digits (entries)
  (loop for entry in entries
	sum (loop for n in (decode-entry entry *decode-maps*)
		  when (member n (list 1 4 7 8))
		    sum 1)))

;; Part 2.
(defun sum-outputs (entries)
  (loop for entry in entries
	sum (parse-integer (format nil "~{~a~}" (decode-entry entry *decode-maps*)))))

(defun decode-entry (entry decode-maps)
  (loop for map in decode-maps
	when (valid-decoding? map (entry-inputs entry))
	  return (loop for sig in (entry-outputs entry)
		       collect (decode-to-digit map sig))))

(defun valid-decoding? (decode-map signals)
  ;; Can all of the signals be mapped to a digit?
  (equalp
   (sort (loop for (digit digit-signal) in *digit-specs*
	       collect digit-signal)
	 #'string<=)
   (sort (loop for s in signals
	       collect (decode-signal decode-map s))
	 #'string<=)))

(defun decode-signal (decode-map sig)
  (format nil
	  "~{~a~}"
	  (sort
	   (loop for c across sig
		 collect (car (rassoc c decode-map)))
	   #'char<=)))

(defun decode-to-digit (map sig)
  (let ((decoded-sig (decode-signal map sig)))
    (car
     (find-if (lambda (digit-spec)
		(string= (cadr digit-spec) decoded-sig))
	      *digit-specs*))))
