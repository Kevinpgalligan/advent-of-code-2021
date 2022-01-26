(defparameter *costs*
  '((A . 1)
    (B . 10)
    (C . 100)
    (D . 1000)))

(defun cost (amphipod-type steps)
  (* steps (cdr (assoc amphipod-type *costs*))))

(defclass graph ()
  ((edges
    :initform (make-hash-table :test 'equalp))
   (depth
    :initarg :depth
    :reader depth)))

(defun add-edge! (g A B moves)
  (with-slots (edges)
      g
    (setf (gethash A edges) (cons (list B moves) (gethash A edges)))
    (setf (gethash B edges) (cons (list A moves) (gethash B edges)))))

(defun get-neighbours (g node)
  (with-slots (edges) g
    (gethash (symbol-name node) edges)))

(defun make-graph (edges depth)
  (let ((g (make-instance 'graph :depth depth)))
    (loop for edge in edges
	  do (add-edge! g
			(first edge)
			(second edge)
			(if (= 3 (length edge)) (third edge) 1)))
    g))

(defun make-burrow-graph (depth)
  (let ((g (make-instance 'graph :depth depth)))
    (loop for edge in '((LCL LCR)
			(LCR A1 2)
			(LCR A-B 2)
			(A1 A-B 2)
			(A-B B1 2)
			(A-B B-C 2)
			(B1 B-C 2)
			(B-C C1 2)
			(B-C C-D 2)
			(C1 C-D 2)
			(C-D D1 2)
			(D1 RCL 2)
			(C-D RCL 2)
			(RCL RCR))
	  do (add-edge! g
			(first edge)
			(second edge)
			(if (= 3 (length edge)) (third edge) 1)))
    (loop for pod in '(A B C D)
	  do (loop for k = 1 then (1+ k)
		   while (< k depth)
		   do (add-edge!
		       g
		       (intern (concatenate 'string (symbol-name pod) (write-to-string k)))
		       (intern (concatenate 'string (symbol-name pod) (write-to-string (1+ k))))
		       1)))
    g))

(defun search-state (positions energy)
  (list positions energy))
(defun state-positions (state) (first state))
(defun state-energy (state) (second state))

(defmacro insert-state! (search-stack state)
  `(let ((state ,state))
    (if (or (null ,search-stack)
	     (<= (state-energy state) (state-energy (first ,search-stack))))
	 (push state ,search-stack)
	 (loop for rest on ,search-stack
	       do (when (or (null (cdr rest))
			    (<= (state-energy state) (state-energy (cadr rest))))
		    (let ((new-cons (cons state (cdr rest))))
		      (setf (cdr rest) new-cons))
		    (return))))))

(defun parse-input (path)
  (with-open-file (in path)
    (read-line in)
    (read-line in)
    (apply #'nconc
	   (loop for line = (read-line in nil nil)
		 for depth = 1 then (1+ depth)
		 while line
		 collect (loop for pod-as-str in (remove-if (lambda (s)
							      (not (member s '("A" "B" "C" "D") :test 'string=)))
							    (split-string "#" line))
			       for room-type in '(A B C D)
			       collect (list (intern pod-as-str)
					     (intern
					      (concatenate 'string (symbol-name room-type) (write-to-string depth)))))))))

(defun split-string (delimit string)
  (let ((i 0))
    (loop while (< i (length string))
	  collect (let* ((end-index (or (search delimit string :start2 i) (length string)))
			 (substr (subseq string i end-index)))
		    (setf i (+ end-index (length delimit)))
		    substr))))

(defun solve (positions)
  (let* (lowest-cost
	 (graph (make-burrow-graph (max 2 (max-depth positions))))
	 (canonical-positions (positions-canonical-form positions))
	 (search-stack (list (search-state canonical-positions 0)))
	 (seen-positions (make-hash-table :test 'equalp)))
    (setf (gethash canonical-positions seen-positions) 0)
    (loop while search-stack
	  do (let ((next (pop search-stack)))
	       (when (or (null lowest-cost)
			 (< (state-energy next) lowest-cost))
		 (if (all-amphipods-in-position? (state-positions next))
		     (setf lowest-cost (state-energy next))
		     (loop for child-state in (generate-child-states next graph)
			   do (when (and (or (null lowest-cost)
					     (< (state-energy child-state) lowest-cost))
					 (or (not (gethash (state-positions child-state) seen-positions))
					     (< (state-energy child-state)
						(gethash (state-positions child-state) seen-positions))))
				(setf (gethash (state-positions child-state) seen-positions)
				      (state-energy child-state))
				;; Not optimal, may have to search this state's
				;; children again!
				(insert-state! search-stack child-state)))))))
    lowest-cost))

(defun max-depth (positions)
  (loop for (pod pos) in positions
	maximize (let ((s (symbol-name pos)))
		   (digit-char-p (aref s (1- (length s)))))))

(defun all-amphipods-in-position? (positions)
  (loop for (pod position) in positions
	always (destination-room? pod position)))

(defun destination-room? (pod pos)
  (member pos
	  (list (lower-home-position pod) (upper-home-position pod))
	  :test 'pos=))

(defun lower-home-position (pod)
  (make-symbol (concatenate 'string (symbol-name pod) "2")))

(defun upper-home-position (pod)
  (make-symbol (concatenate 'string (symbol-name pod) "1")))

(defun generate-child-states (state graph)
  (let ((positions (state-positions state)))
    (apply #'nconc
	   (loop for i below (length positions)
		 collect (destructuring-bind (pod pos)
			     (nth i positions)
			   (loop for (new-pos energy) in (find-next-positions pod pos positions graph)
				 collect (search-state (positions-amended positions i (list pod new-pos))
						       (+ energy (state-energy state)))))))))

(defun positions-amended (positions i new-value)
  (let ((new-positions (copy-positions positions)))
    (setf (nth i new-positions) new-value)
    (positions-canonical-form new-positions)))

(defun positions-canonical-form (positions)
  ;; So that positions are compared properly, sort them first
  ;; by amphipod type and then by position.
  (sort positions
	(lambda (pod-pos-1 pod-pos-2)
	  (destructuring-bind (pod1 pos1) pod-pos-1
	    (destructuring-bind (pod2 pos2) pod-pos-2
	      (or (pos< pod1 pod2)
		  (and (pos= pod1 pod2)
		       (pos< pos1 pos2))))))))

(defun copy-positions (positions)
  (loop for pod-pos in positions
	collect (copy-list pod-pos)))

(defmacro queue-insert! (queue x)
  `(let ((x ,x))
     (if (null ,queue)
	 (push x ,queue)
	 (loop for rest on ,queue
	       do (when (null (cdr rest))
		    (setf (cdr rest) (cons x nil))
		    (return))))))

(defun find-next-positions (pod pos positions graph)
  ;; BFS to find valid moves for an amphipod.
  ;; Returns list of pairs of (new-pos energy).
  ;; Due to the structure of the graph, BFS should find
  ;; the shortest path to each new position, even though
  ;; edges have different costs.
  (if (fit-into-final-position? pod pos positions (depth graph))
      ;; Either the amphipod is already in the lower position of its
      ;; room, or it's in the upper position and the lower position is
      ;; occupied by the other amphipod of this type. There's no point
      ;; moving anymore, as it will add energy without making progress.
      nil
      (let ((next-positions (list))
	    (seen (list pos))
	    (queue (list (list pos 0))))
	(loop while queue
	      do (destructuring-bind (current moves-so-far)
		     (pop queue)
		   (loop for (n moves) in (get-neighbours graph current)
			 when (and (not (pos= n pos))
				   (not (member n seen :test 'pos=))
				   (not (position-occupied? n positions)))
			   do (let ((total-moves (+ moves moves-so-far)))
				(push (list n (cost pod total-moves)) next-positions)
				(push n seen)
				(queue-insert! queue (list n total-moves))))))
	(loop for (next-pos cost) in next-positions
	      when (not
		    (or
		     (and (hallway? pos)
			  (hallway? next-pos))
		     (and (room? next-pos)
			  (not (destination-room? pod next-pos)))
		     (and (destination-room? pod next-pos)
			  (not (fit-into-final-position? pod next-pos positions (depth graph))))))
		collect (list next-pos cost)))))

(defun fit-into-final-position? (pod pos positions depth)
  (and (destination-room? pod pos)
       (loop for lower-pos in (lower-destination-room-positions pos depth)
	     always (pos= pod (position-occupied? lower-pos positions)))))

(defun hallway? (pos)
  (loop for pod in '(A B C D)
	never (destination-room? pod pos)))

(defun room? (pos)
  (loop for pod in '(A B C D)
	  thereis (destination-room? pod pos)))

(defun lower-destination-room-positions (pos depth)
  (let ((name (symbol-name pos)))
    (loop for k = (1+ (parse-integer (subseq name (1- (length name)) (length name))))
	    then (1+ k)
	  while (<= k depth)
	  collect (make-symbol (concatenate 'string (subseq name 0 1) (write-to-string k))))))

;; These functions are used to compare amphipod & position labels.
;; They should prob have better names.
(defun pos= (p1 p2)
  (string= (symbol-name p1) (symbol-name p2)))

(defun pos< (p1 p2)
  (string< (symbol-name p1) (symbol-name p2)))

(defun position-occupied? (pos positions)
  (loop for (pod other-pos) in positions
	when (pos= pos other-pos)
	  return pod))
