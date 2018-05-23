(defparameter test-img nil)


(defun reset-test-img (&key (resolution '(32 32)) (color 255))
  (setf test-img (make-8-bit-gray-image (car resolution) (cadr resolution)))
  (let ((out-img (output-image "~/test.png")))
    (fill-image test-img color)
    (write-png-file out-img test-img) test-img))


(defun test-draw-line () 
  (reset-test-img)
  (let* ((out (output-image "~/test.png")) 
	 (inp (read-png-file out)))
    (setf test-img inp)
    (time
     (draw-line test-img 16 8 0 0))
    (write-png-file out test-img)))


(defun draw-line (img x0 y0 x1 y1 &key (color 0))
  (multiple-value-bind (first-x first-y second-x second-y)
      (if (< x0 x1)
	  (values x0 y0 x1 y1)
	  (values x1 y1 x0 y0))
    (let* ((dx (- second-x first-x))
	   (dy (- second-y first-y))
	   (err 0)
	   (steep (> dy dx)))
      (multiple-value-bind (new-x0 new-y0 new-x1 new-y1 new-dx new-dy)
	  (if steep
	      (values first-y first-x second-y second-x dy dx)
	      (values first-x first-y second-x second-y dx dy))
	(let ((y new-y0))
	  (loop for x from new-x0 upto new-x1 
	     do 
	       (setf err (+ err new-dy))
	       (if steep
		   (setf (pixel img x y) color)
		   (setf (pixel img y x) color))
	       (when (>= err new-dx)
		 (setf y (1+ y))
		 (setf err (- err new-dx)))))
	img))))


(defun parse-obj-vertex-line (line)
  (cl-ppcre:register-groups-bind (x y z w)
					;("^v (-?[0-9\.]*) (-?[0-9\.]*) (-?[0-9\.]*) ?(-?[0-9\.]*)?" line)
      ("^v ([^ ]*) ([^ ]*) ([^ ]*) ?([^ ]*)?" line)
    (list x y z (if (> (length w) 0) w "1.0"))))


(defparameter face-regex-triple
  (concatenate 'string
	       "^f "
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)? "
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)? "
	       "([0-9]*)/?([0-9]*)?/?([0-9]*)?"))

(defun parse-obj-face-line (line)
  (cl-ppcre:register-groups-bind (v1 vt1 vn1 v2 vt2 vn2 v3 vt3 vn3)
      (face-regex-triple line)
    (list (list v1 v2 v3)
	  (list vt1 vt2 vt3)
	  (list vn1 vn2 vn3))))


(defmacro let-cond (&body body)
  (when (car body)
    `(let ((result ,(caar body)))
       (if result
	   ,(car (cdr (car body)))
	   (let-cond
	     ,@(cdr body))))))


(defun parse-float (str)
  (with-input-from-string (stream str)
    (read stream)))


(defun read-obj-file (path)
  (let ((in (open path :if-does-not-exist nil))
	(vertices (make-array 0 :adjustable t :fill-pointer t))
	(faces (make-array 0 :adjustable t :fill-pointer t)))
    (when in
      (loop for line = (read-line in nil)
	 while line do
	   (let-cond
	     ((parse-obj-vertex-line line)
	      (vector-push-extend
	       (mapcar #'parse-float result) vertices))
	     ((parse-obj-face-line line)
	      (vector-push-extend
	       (loop for triple in result
		  collect (mapcar #'parse-integer triple))
	       faces)))))
    (values vertices faces)))


(defun straight-on (vertex)
  (list (car vertex) (cadr vertex)))


(defun 11to01 (num)
  (/ (1+ num) 2))


(defun vertex->pixel (vertex
		      &key
			(resolution '(1024 1024))
			(projection #'straight-on))
  (let* ((projected-vertex (funcall projection vertex))
	 (max-pixel-x (1- (car resolution)))
	 (max-pixel-y (1- (cadr resolution)))
	 (x (round (* (11to01 (car projected-vertex))
		      max-pixel-x)))
	 (y (round (* (11to01 (cadr projected-vertex))
		      max-pixel-y)))
	 (flipped-y (- max-pixel-y y)))
    (list x flipped-y)))


(defun wire-render (vertices faces img
		    &key
		      (resolution '(1024 1024))
		      (projection #'straight-on)
		      (color 0))
  (loop for face in (coerce faces 'list)
     do
       (let ((sides 3))
	 (loop for i from 0 to (1- sides)
	    do
	      (let* ((v-indices (car face))
		     (v1-index (elt v-indices i))
		     (v2-index (elt v-indices (mod (1+ i) sides)))
		     (v1 (elt vertices (1- v1-index)))
		     (v2 (elt vertices (1- v2-index)))
		     (projected-v1 (vertex->pixel v1
						  :resolution resolution
						  :projection projection))
		     (projected-v2 (vertex->pixel v2
						  :resolution resolution
						  :projection projection))
		     (x1 (first projected-v1))
		     (y1 (second projected-v1))
		     (x2 (first projected-v2))
		     (y2 (second projected-v2)))
		(draw-line img x1 y1 x2 y2 :color color))))))


(defun test-wireframe ()
  (let ((resolution '(1024 1024)))
    (reset-test-img :resolution resolution :color 0)
    (let* ((out (output-image "~/test.png")) 
	   (inp (read-png-file out)))
      (setf test-img inp)
      (multiple-value-bind (vertices faces)
	  (read-obj-file "~/quicklisp/local-projects/3d-render/head.obj")
	(time
	 (wire-render vertices faces test-img
		      :resolution resolution
		      :color 255)))
      (write-png-file out test-img))))
