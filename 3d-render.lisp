(defparameter test-img nil)


(defun reset-test-img (&key (resolution '(32 32)) (color 255))
  (setf test-img (make-8-bit-gray-image (first resolution) (second resolution)))
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
  (let* ((dx (- x1 x0))
	 (dy (- y1 y0))
	 (steep (> (abs dy) (abs dx))))
    (multiple-value-bind (new-x0 new-y0 new-x1 new-y1 new-dx new-dy)
	(if steep
	    (values y0 x0 y1 x1 dy dx)
	    (values x0 y0 x1 y1 dx dy))
      (multiple-value-bind (first-x first-y second-x second-y final-dx final-dy)
	  (if (< new-x0 new-x1)
	      (values new-x0 new-y0 new-x1 new-y1 new-dx new-dy)
	      (values new-x1 new-y1 new-x0 new-y0 (- 0 new-dx) (- 0 new-dy)))
	(let ((y first-y)
	      (err 0))
	  (loop for x from first-x upto second-x 
	     do 
	       (setf err (+ err final-dy))
	       (if steep
		   (setf (pixel img x y) color)
		   (setf (pixel img y x) color))
	       (when (>= (abs err) final-dx)
		 (if (>= final-dy 0)
		     (progn
		       (setf y (1+ y))
		       (setf err (- err final-dx)))
		     (progn
		       (setf y (1- y))
		       (setf err (+ err final-dx))))))))))
  img)


(defun parse-vertex-line (line)
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

(defun parse-face-line (line)
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
	     ((parse-vertex-line line)
	      (vector-push-extend
	       (mapcar #'parse-float result) vertices))
	     ((parse-face-line line)
	      (vector-push-extend
	       (loop for triple in result
		  collect (mapcar #'parse-integer triple))
	       faces)))))
    (values vertices faces)))


(defun straight-on (vertex)
  (list (first vertex) (second vertex)))
(defun right-side (vertex)
  (list (third vertex) (second vertex)))


(defun 11to01 (num)
  (/ (1+ num) 2))


(defun vertex->pixel (vertex
		      &key
			(resolution '(1024 1024))
			(projection #'straight-on))
  (let* ((projected-vertex (funcall projection vertex))
	 (max-pixel-x (1- (first resolution)))
	 (max-pixel-y (1- (second resolution)))
	 (x (round (* (11to01 (first projected-vertex))
		      max-pixel-x)))
	 (y (round (* (11to01 (second projected-vertex))
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


(defun refresh-image ()
  (let ((out (output-image "~/test.png")))
    (write-png-file out test-img)))

(defun test-wireframe ()
  (let ((resolution '(1024 1024)))
    (reset-test-img :resolution resolution :color 0)
    (multiple-value-bind (vertices faces scale)
	(read-obj-file "~/quicklisp/local-projects/3d-render/obj/mini-cooper.obj")
      ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/head.obj")
      (time
       (wire-render vertices faces test-img
		    :resolution resolution
		    ;;:projection #'straight-on
		    :projection #'right-side
		    :color 255)))
    (refresh-image)))
