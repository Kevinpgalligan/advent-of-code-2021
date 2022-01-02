(defun read-map (path)
  (let ((graph (make-instance 'graph)))
   (with-open-file (in path)
     (loop for line = (read-line in nil nil)
	   while line
	   do (let ((separator-index (search "-" line)))
		(graph-add-edge graph
				(subseq line 0 separator-index)
				(subseq line (1+ separator-index))))))
    graph))

(defclass graph ()
  ((edges
    :initarg :edges
    :initform (make-hash-table :test 'equalp)
    :reader edges)))

(defun graph-add-edge (graph n1 n2)
  (push n1 (gethash n2 (edges graph)))
  (push n2 (gethash n1 (edges graph))))

(defun graph-get-neighbours (graph node)
  (gethash node (edges graph)))

(defun graph-nodes (graph)
  (loop for n being each hash-key of (edges graph)
	collect n))

(defun serialise-graph (graph path)
  (with-open-file (out path :direction :output)
    (format out "strict graph G {~%")
    (loop for node in (graph-nodes graph)
	  do (format out "  ~a [shape=circle,label=\"~a\"]~%" node node))
    (loop for node in (graph-nodes graph)
	  do (loop for neighbour in (graph-get-neighbours graph node)
		   do (format out "  ~a -- ~a~%" node neighbour)))
    (format out "}~%")))

(defun count-paths (graph)
  (dfs-count graph "start"))

(defun dfs-count (graph node &key visited double-visit)
  (when (not visited)
    (push node visited))
  (if (string= node "end")
      1
      (loop for neigh in (remove-if (lambda (neigh)
				      (and (member neigh visited :test #'string=)
					   (or (string= neigh "start")
					       double-visit)))
				    (graph-get-neighbours graph node))
	    sum (dfs-count graph
			   neigh
			   :visited (if (big-cave? neigh) visited (cons neigh visited))
			   :double-visit (or double-visit (member neigh visited :test #'string=))))))

(defun big-cave? (node)
  (string= node (string-upcase node)))
