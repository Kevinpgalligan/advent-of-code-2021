(declaim (optimize (debug 3)))

(defclass packet ()
  ((version
    :initarg :version
    :reader version)
   (ptype
    :initarg :ptype
    :reader ptype)
   (contents
    :initarg :contents
    :reader contents)
   (bits
    :initarg :bits
    :reader bits)))

(defun read-packets (hex)
  (let* ((bytes (make-array (ceiling (* 4 (length hex)) 8)
			    :initial-element 0
			    :element-type 'unsigned-byte)))
    (loop for i = 0 then (+ 2 i)
	  while (< i (length hex))
	  do (let ((b1 (hex-decode (aref hex i)))
		   (b2 (and (< (1+ i) (length hex))
			    (hex-decode (aref hex (1+ i))))))
	       (setf (aref bytes (/ i 2))
		     (logxor (ash b1 4)
			     (or b2 0)))))
    (parse-packet (bitstream bytes))))

(defparameter *hex-digits*
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
    #\8 #\9 #\A #\B #\C #\D #\E #\F))

(defun hex-decode (c)
  (loop for d in *hex-digits*
	for i = 0 then (1+ i)
	when (char= d c)
	  return i))

(defun bitstream (bytes)
  (let ((i 0))
    (lambda (cmd)
      (cond
	((numberp cmd)
	 (let ((bits-to-be-read cmd)
	       (result 0))
	   (loop while (> bits-to-be-read 0)
		 do (let* ((byte-index (truncate i 8))
			   (remaining-bits-in-byte (- 8 (mod i 8)))
			   (bits-to-read-from-byte
			     (min remaining-bits-in-byte bits-to-be-read)))
		      ;; Make space for the next bits!
		      (setf result (ash result bits-to-read-from-byte))
		      (setf result
			    (logxor result
				    ;; Mod to remove the bits we've already
				    ;; consumed, shift to get rid of the bits
				    ;; we don't want to consume yet.
				    (ash
				     (mod (aref bytes byte-index)
					  (expt 2 remaining-bits-in-byte))
				     (- bits-to-read-from-byte
					remaining-bits-in-byte))))
		      (incf i bits-to-read-from-byte)
		      (decf bits-to-be-read bits-to-read-from-byte))
		 finally (return result))))
	((eq cmd 'remaining)
	 (< i (* 8 (length bytes))))))))

(defun consume-bits (bitstream n)
  (funcall bitstream n))

(defun bits-remaining-p (bitstream)
  (funcall bitstream 'remaining))

(defun parse-packet (bitstream)
  (let ((version (consume-bits bitstream 3))
	(id (consume-bits bitstream 3)))
    (case id
      (4 (multiple-value-bind (literal-value literal-bits)
	     (parse-literal bitstream)
	   (make-instance 'packet
			  :version version
			  :ptype 'literal
			  :contents literal-value
			  :bits (+ 6 literal-bits))))
      (otherwise
       (multiple-value-bind (subpackets subpacket-bits)
	   (parse-subpackets bitstream)
	 (make-instance 'packet
			:version version
			:ptype (operatorid->type id)
			:contents subpackets
			:bits (+ 6 subpacket-bits)))))))

(defun operatorid->type (id)
  (case id
    (0 'sum)
    (1 'product)
    (2 'minimum)
    (3 'maximum)
    (5 'greater-than)
    (6 'less-than)
    (7 'equal-to)))

(defun parse-subpackets (bitstream)
  (let ((length-type-id (consume-bits bitstream 1)))
    (case length-type-id
      (0 (let ((total-length 0)
	       (target-length (consume-bits bitstream 15)))
	   (values
	    (loop while (< total-length target-length)
		  collect (let ((subpacket (parse-packet bitstream)))
			    (incf total-length (bits subpacket))
			    subpacket))
	    (+ 16 total-length))))
      (1 (let* ((num-subpackets (consume-bits bitstream 11))
		(subpackets (loop repeat num-subpackets
				  collect (parse-packet bitstream))))
	   (values
	    subpackets
	    (+ 12 (loop for subpacket in subpackets
			sum (bits subpacket))))))
      (otherwise (error "unknown length type ID")))))

(defun parse-literal (bitstream)
  (let ((result 0)
	(bits 0))
    (loop do (let ((next-bits (consume-bits bitstream 5)))
	       (incf bits 5)
	       ;; Take first 4 bits as the value.
	       (setf result (logxor (ash result 4) (mod next-bits (expt 2 4))))
	       (when (zerop (logand next-bits #b10000))
		 (return))))
    (values result bits)))

(defun sum-packet-versions (packet)
  (case (ptype packet)
    (literal (version packet))
    (operator (+ (version packet)
		 (loop for subpacket in (contents packet)
		       sum (sum-packet-versions subpacket))))
    (otherwise (error "unknown packet type"))))

(defun evaluate-packet (packet)
  (if (eq 'literal (ptype packet))
      (contents packet)
      (let ((subvalues (mapcar #'evaluate-packet (contents packet))))
	(case (ptype packet)
	  (sum (reduce #'+ subvalues))
	  (product (reduce #'* subvalues))
	  (minimum (apply #'min subvalues))
	  (maximum (apply #'max subvalues))
	  (greater-than (bool->int (> (first subvalues) (second subvalues))))
	  (less-than (bool->int (< (first subvalues) (second subvalues))))
	  (equal-to (bool->int (= (first subvalues) (second subvalues))))
	  (otherwise (error "unknown operation type"))))))

(defun bool->int (x)
  (if x 1 0))
