(defun pair (x y) (list x y))
(defun left (pair) (first pair))
(defun right (pair) (second pair))

(defun parse-snailfish-number (s)
  ())

(defun solve (path)
  (with-open-file (in path)
    (let (result)
      (loop for line = (read-line in nil nil)
	    while line
	    do (setf result (snailfish-add result
					   (snailfish-reduce!
					    (parse-snailfish-number line)))))
      (snailfish-magnitude result))))

(defun snailfish-add (x y)
  (pair x y))

(defun snailfish-magnitude (n)
  (if (numberp n)
      n
      (+ (* 3 (snailfish-magnitude (left n)))
	 (* 2 (snailfish-magnitude (right n))))))

(defun snailfish-reduce! (sn)
  (loop repeat
	while (or (snailfish-explode-nested! sn)
		  (snailfish-split-number! sn)))
  sn)

(defun snailfish-explode-nested! (sn &key (depth 0))
  ())
