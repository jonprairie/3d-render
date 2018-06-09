(defun parse-vertex-line (line)
  (cl-ppcre:register-groups-bind (x y z w)
      ("^v +([^ ]*) +([^ ]*) +([^ ]*) *([^ ]*)" line)
    ;;("^v +([0-9\.-]*) +([0-9\.-]*) +([0-9\.-]*) *?([0-9\.-]*)?" line)
    (make-v :x (parse-float x)
	    :y (parse-float y)
	    :z (parse-float z))))


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


(defun parse-vertex-texture-line (line)
  (cl-ppcre:register-groups-bind (u v w)
      ("^vt +([^ ]*) +([^ ]*) *([^ ]*)?" line)
    (make-v :x (parse-float u)
	    :y (parse-float v)
	    :z (parse-float w))))


(defun parse-empty-line (line)
  (cl-ppcre:scan "(^$|^ *$|^#)" line))


(defun read-obj-file (path)
  ;; reads a .obj file at PATH and returns a model struct
  (let ((in (open path :if-does-not-exist nil))
	(vertices (make-array 0 :adjustable t :fill-pointer t))
	(vertex-textures (make-array 0 :adjustable t :fill-pointer t))
	(faces (make-array 0 :adjustable t :fill-pointer t))
	(scale 0))
    (when in
      (loop for line = (read-line in nil) while line do
	   (let-cond
	     ((parse-empty-line line)
	      nil)
	     ((parse-vertex-line line)
	      (let ((max-coord (max (abs (x result))
				    (abs (y result))
				    (abs (z result)))))
		(when (> (abs max-coord) scale)
		  (setf scale (abs max-coord))))
	      (vector-push-extend result vertices))
	     ((parse-vertex-texture-line line)
	      (vector-push-extend result vertex-textures))
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
      (close in)
      (make-model
       :triangles (mapcar
		   (lambda (x)
		     (let ((face (mapcar (lambda (x) (- x 1)) (first x))))
		       (make-triangle :v0 (v/ (aref vertices (first face))
					      scale)
				      :v1 (v/ (aref vertices (second face))
					      scale)
				      :v2 (v/ (aref vertices (third face))
					      scale))))
		   (coerce faces 'list))
       :scale scale))))
