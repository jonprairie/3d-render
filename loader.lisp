(defun parse-vertex-line (line)
  (cl-ppcre:register-groups-bind (x y z w)
      ("^v +([^ ]*) +([^ ]*) +([^ ]*) *?([^ ]*)?" line)
    (list x y z (if (> (length w) 0) w "1.0"))))


(defparameter face-regex-triple
  (concatenate 'string
	       "^f +"
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)? +"
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)? +"
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)?"))


(defun parse-face-line (line)
  (cl-ppcre:register-groups-bind (v0 vt0 vn0 v1 vt1 vn1 v2 vt2 vn2)
      (face-regex-triple line)
    (list (list v0 v1 v2)
	  (when (> (length vt0))
	    (list vt0 vt1 vt2))
	  (when (> (length vn0))
	    (list vn0 vn1 vn2)))))


(defun parse-empty-line (line)
  (cl-ppcre:scan "(^$|^ *$|^#)" line))


(defun read-obj-file (path)
  (let ((in (open path :if-does-not-exist nil))
	(vertices (make-array 0 :adjustable t :fill-pointer t))
	(faces (make-array 0 :adjustable t :fill-pointer t))
	(scale 0))
    (when in
      (loop for line = (read-line in nil)
	 while line do
	   (let-cond
	     ((parse-empty-line line)
	      nil)
	     ((parse-vertex-line line)
	      (let ((vertex-floats (mapcar #'parse-float result)))
		(loop for coord in vertex-floats
		   do
		     (when (> (abs coord) scale)
		       (setf scale (abs coord))))
		(vector-push-extend
		 vertex-floats vertices)))
	     ((parse-face-line line)
	      (vector-push-extend
	       (loop for triple in result
		  collect (mapcar
			   #'(lambda (x)
			       (if (= (length x) 0)
				   nil
				   (parse-integer x)))
			   triple))
	       faces))))
      (close in))
    (list vertices faces scale)))


