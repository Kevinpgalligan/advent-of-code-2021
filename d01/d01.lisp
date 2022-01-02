(defun read-numberfile (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (parse-integer line))))

(defun count-increments (ns &key (window-size 1))
  (let* ((length (length ns))
	 (sums (loop for remaining on ns
		     for i from 0 below length
		     when (<= (+ i window-size) length)
		       collect (reduce #'+ (subseq remaining 0 window-size)))))
    (loop for (s1 . (s2 . rest)) on sums
	  when (and (not (null s2))
		    (< s1 s2))
	    sum 1)))
