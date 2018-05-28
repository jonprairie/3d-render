(defparameter test-img nil)


(defun reset-test-img (&key (resolution '(32 32)) (color '(255 255 255 255)))
  (setf test-img (opticl::make-8-bit-rgba-image
		  (first resolution)
		  (second resolution)))
  (let ((out-img (output-image "~/test.png")))
    (apply #'fill-image (cons test-img color))
    (write-png-file out-img test-img) test-img))


(defun draw-line (img v0 v1 &key (color '(255 255 255 255)))
  (let ((points-list (enum-pixels-on-line v0 v1)))
    (loop for point in points-list do
	 (let ((x (first point))
	       (y (second point)))
	   (setf (pixel img y x) (values-list color))))))


(defun test-draw-line () 
  (reset-test-img)
  (let* ((out (output-image "~/test.png")) 
	 (inp (read-png-file out)))
    (setf test-img inp)
    (time
     (draw-line test-img '(16 8) '(0 0)))
    (write-png-file out test-img)))


(defun draw-horizontal-line (img v0 v1
			     &key
			       (z-buffer nil)
			       (color '(255 255 255 255)))
  (multiple-value-bind (first-v second-v)
      (if (< (first v0) (first v1))
	  (values v0 v1)
	  (values v1 v0))
    (let* ((x0 (first first-v))
	   (x1 (first second-v))
	   (y0 (second first-v))
	   (y1 (second second-v))
	   (z0 (third first-v))
	   (z1 (third second-v))
	   (curr-z z0)
	   (x-step (- x1 x0))
	   (z-step (if (= x-step 0)
		       0
		       (/ (- z1 z0) (- x1 x0)))))
      (loop for x from x0 to x1 do
	   (cond
	     ((null z-buffer)
	      (setf (pixel img y0 x) (values-list color)))
	     ((< curr-z (aref z-buffer x y0))
	      (setf (pixel img y0 x) (values-list color))
	      (setf (aref z-buffer x y0) curr-z)))
	   (incf curr-z z-step))))
  z-buffer)


(defun draw-shaded-triangle (img v0 v1 v2
			     &key
			       (z-buffer nil)
			       (color '(255 255 255 255)))
  (let ((horizontal-slices (enum-horizontal-slices-on-triangle (list v0 v1 v2))))
    (loop for y being the hash-keys in horizontal-slices do
	 (let* ((x-list (gethash y horizontal-slices))
		(len (length x-list)))
	   (cond 
	     ((> len 0)
	      (setf z-buffer (draw-horizontal-line img
						   (first x-list)
						   (second x-list)
						   :z-buffer z-buffer
						   :color color)))
	     (t (error "x-list length should be 1 or greater"))))))
  z-buffer)


(defun splice-z (pixel z)
  (append pixel (list z)))


(defun enum-horizontal-slices-on-triangle (triangle)
  (let ((horizontal-slices (make-hash-table :test 'eq)))
    (loop for i from 0 to 2 do
	 (let* ((first-index i)
		(second-index (mod (1+ i) 3))
		(line (enum-pixels-on-line (elt triangle first-index)
					   (elt triangle second-index)))
		(num-pixels (1- (length line)))
		(z0 (third (elt triangle first-index)))
		(z1 (third (elt triangle second-index)))
		(z-step (if (= num-pixels 0)
			    0
			    (/ (- z1 z0) num-pixels)))
		(curr-z z0))
	   (loop for pixel in line do
		(let* ((y (second pixel))
		       (curr-horizontal-slice (gethash y horizontal-slices)))
		  (cond
		    ((null curr-horizontal-slice)
		     (setf (gethash y horizontal-slices)
			   (list (splice-z pixel curr-z))))
		    ((= (length curr-horizontal-slice) 1)
		     (if (< (first pixel) (first (first curr-horizontal-slice)))
			 (setf (gethash y horizontal-slices)
			       (append (list (splice-z pixel curr-z))
				       curr-horizontal-slice))
			 (setf (gethash y horizontal-slices)
			       (append curr-horizontal-slice
				       (list (splice-z pixel curr-z))))))
		    ((= (length curr-horizontal-slice) 2)
		     (cond
		       ((< (first pixel) (first (first curr-horizontal-slice)))
			(setf (gethash y horizontal-slices)
			      (append (list (splice-z pixel curr-z))
				      (cdr curr-horizontal-slice))))
		       ((> (first pixel) (first (second curr-horizontal-slice)))
			(setf (gethash y horizontal-slices)
			      (append (list (car curr-horizontal-slice))
				      (list (splice-z pixel curr-z)))))))
		    (t (error "horizontal slice should have length 0, 1 or 2"))))
		(setf curr-z (incf curr-z z-step)))))
    horizontal-slices))


(defun enum-pixels-on-line (v0 v1)
  (let* ((point-list nil)
	 (x0 (first v0))
	 (y0 (second v0))
	 (x1 (first v1))
	 (y1 (second v1))
	 (dx (- x1 x0))
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
		   (setf point-list (cons (list y x) point-list))
		   (setf point-list (cons (list x y) point-list)))
	       (when (>= (abs err) final-dx)
		 (if (>= final-dy 0)
		     (progn
		       (setf y (1+ y))
		       (setf err (- err final-dx)))
		     (progn
		       (setf y (1- y))
		       (setf err (+ err final-dx)))))))))
    (nreverse point-list)))


(defun parse-vertex-line (line)
  (cl-ppcre:register-groups-bind (x y z w)
      ;;("^v (-?[0-9\.]*) (-?[0-9\.]*) (-?[0-9\.]*) ?(-?[0-9\.]*)?" line)
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


(defmacro let-cond (&body body)
  (when (car body)
    `(let ((result ,(caar body)))
       (if result
	   (progn
	     ,(car (cdr (car body))))
	   (let-cond
	     ,@(cdr body))))))


(defun parse-float (str)
  (with-input-from-string (stream str)
    (read stream)))


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


(defparameter straight-on '((1 0 0)
			    (0 1 0)))
(defparameter right-side '((0 0 1)
			   (0 1 0)))
(defparameter left-side '((0 0 -1)
			  (0 1 0)))
(defparameter angled-pi/4 '((.707 0 -.707)
			    (0 1 0)))
(defparameter angled-3pi/4 '((.707 0 -.707)
			     (0 1 0)))
(defparameter stupid '((.707 0 -.707)
		       (-.577 .577 -.577)))
(defparameter bottom-stupid '((.707 -.707 0)
			      (0 0 1)))
(defparameter bottom '((1 0 0)
		       (0 0 1)))


(defun 11to01 (num)
  (/ (1+ num) 2))


(defun dot (v0 v1)
  (reduce #'+
	  (map (type-of v0) #'* v0 v1)))


(defun cross (v0 v1)
  (let ((x0 (first v0))
	(x1 (first v1))
	(y0 (second v0))
	(y1 (second v1))
	(z0 (third v0))
	(z1 (third v1)))
    (list (- (* y0 z1) (* z0 y1))
	  (- (* z0 x1) (* x0 z1))
	  (- (* x0 y1) (* y0 x1)))))


(defun vertex->pixel (vertex
		      &key
			(resolution '(1024 1024))
			(projection straight-on)
			(scale 1))
  (let* ((projected-x (dot vertex (first projection)))
	 (projected-y (dot vertex (second projection)))
	 (projected-z (dot vertex (third projection)))
	 (max-pixel-x (1- (first resolution)))
	 (max-pixel-y (1- (second resolution)))
	 (scaled-x (/ projected-x scale))
	 (scaled-y (/ projected-y scale))
	 (x (round (* (11to01 scaled-x) max-pixel-x)))
	 (y (round (* (11to01 scaled-y) max-pixel-y)))
	 (flipped-y (- max-pixel-y y)))
    (list x flipped-y projected-z)))


(defun wire-render (vertices faces img
		    &key
		      (resolution '(1024 1024))
		      (projection straight-on)
		      (color '(255 255 255 255))
		      (scale 1))
  (loop for face in (coerce faces 'list) do
       (let ((sides (length (elt faces 0)))
	     (v-indices (first face)))
	 (loop for i from 0 to (1- sides)
	    do
	      (let* ((v0-index (elt v-indices i))
		     (v1-index (elt v-indices (mod (1+ i) sides)))
		     (v0 (elt vertices (1- v0-index)))
		     (v1 (elt vertices (1- v1-index)))
		     (projected-v0 (vertex->pixel v0
						  :resolution resolution
						  :projection projection
						  :scale scale))
		     (projected-v1 (vertex->pixel v1
						  :resolution resolution
						  :projection projection
						  :scale scale)))
		(draw-line img projected-v0 projected-v1 :color color))))))


(defun sqr (x) (* x x))


(defun normalize (v)
  (if (and (= (first v) 0)
	   (= (second v) 0)
	   (= (third v) 0))
      v
      (let ((len (sqrt (apply #'+ (mapcar #'sqr v)))))
	(mapcar #'(lambda (x) (/ x len)) v))))


(defun get-triangle-light-intensity (triangle light-src)
  (let* ((v0 (first triangle))
	 (v1 (second triangle))
	 (v2 (third triangle))
	 (x0 (first v0))
	 (x1 (first v1))
	 (x2 (first v2))
	 (y0 (second v0))
	 (y1 (second v1))
	 (y2 (second v2))
	 (z0 (third v0))
	 (z1 (third v1))
	 (z2 (third v2))
	 (tri-vector-0 (list (- x2 x0)
			     (- y2 y0)
			     (- z2 z0)))
	 (tri-vector-1 (list (- x1 x0)
			     (- y1 y0)
			     (- z1 z0)))
	 (normal-unit (normalize (cross tri-vector-0 tri-vector-1)))
	 (light-unit (normalize light-src)))
    (dot normal-unit light-unit)))


(defun triangle-render (vertices faces img
			&key
			  (resolution '(1024 1024))
			  (projection straight-on)
			  (color '(255 255 255 255))
			  (scale 1)
			  (light-src nil))
  (let* ((z-buffer (make-array resolution
			       :initial-element most-positive-double-float))
	 (z-vector (cross (second projection) (first projection)))
	 (3d-projection (append projection (list z-vector)))
	 (light-src (if light-src light-src z-vector)))
    (loop for face in (coerce faces 'list) do
	 (let* ((v-indices (first face))
		(v0-index (elt v-indices 0))
		(v1-index (elt v-indices 1))
		(v2-index (elt v-indices 2))
		(v0 (elt vertices (1- v0-index)))
		(v1 (elt vertices (1- v1-index)))
		(v2 (elt vertices (1- v2-index)))
		(light-intensity (get-triangle-light-intensity
				  (list v0 v1 v2)
				  light-src))
		(projected-v0 (vertex->pixel v0
					     :resolution resolution
					     :projection 3d-projection
					     :scale scale))
		(projected-v1 (vertex->pixel v1
					     :resolution resolution
					     :projection 3d-projection
					     :scale scale))
		(projected-v2 (vertex->pixel v2
					     :resolution resolution
					     :projection 3d-projection
					     :scale scale)))
	   (when (< 0 light-intensity)
	     (setf z-buffer
		   (draw-shaded-triangle img
					 projected-v0
					 projected-v1
					 projected-v2
					 :z-buffer z-buffer
					 :color (list (round (* light-intensity 255))
						      (round (* light-intensity 255))
						      (round (* light-intensity 255))
						      255))))))))


(defun refresh-image ()
  (let ((out (output-image "~/test.png")))
    (write-png-file out test-img)))


(defun test-wireframe-render ()
  (let ((resolution '(1024 1024)))
    (reset-test-img :resolution resolution :color '(255 255 255 255))
    (multiple-value-bind (vertices faces scale)
	(time
	 ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/mini-cooper.obj"))
	 (read-obj-file "~/quicklisp/local-projects/3d-render/obj/head.obj"))
      (time
       (wire-render vertices faces test-img
		    :resolution resolution
		    ;;:projection straight-on
		    ;;:projection right-side
		    :projection stupid
		    ;;:projection angled-pi/4
		    :color '(255 255 255 255)
		    :scale scale)))
    (refresh-image)))


(defparameter obj-cache nil)
(defun test-triangle-render (&key (use-cache nil))
  (let ((resolution '(2048 2048)))
    (reset-test-img :resolution resolution :color '(0 0 0 255))
    (destructuring-bind (vertices faces scale)
	(time
	 (if (and use-cache obj-cache)
	     obj-cache
	     (let ((obj
		    ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/mini-cooper.obj")
		    ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/lamp.obj")
		    (read-obj-file "~/quicklisp/local-projects/3d-render/obj/head.obj")
		    ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/diablo.obj")
		     ))
	       (setf obj-cache obj)
	       obj)))
      ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/head.obj"))
      (time
       (triangle-render vertices faces test-img
			:resolution resolution
			;;:projection straight-on
			;;:projection bottom-stupid
			;;:projection bottom
			;;:projection right-side
			;;:projection left-side
			;;:projection stupid
			;;:projection angled-pi/4
			:color '(0 0 0 255)
			:scale scale)))
    (refresh-image)))
