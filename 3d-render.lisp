(defun draw-line (img v0 v1 &key (color '(255 255 255 255)))
  (let ((points-list (enum-pixels-on-line v0 v1)))
    (loop for point in points-list do
	 (let ((x (first point))
	       (y (second point)))
	   (setf (pixel img y x) (values-list color))))))


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
	      (setf z-buffer
		    (draw-horizontal-line img
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
		   (draw-shaded-triangle
		    img
		    projected-v0
		    projected-v1
		    projected-v2
		    :z-buffer z-buffer
		    :color (list (round (* light-intensity 255))
				 (round (* light-intensity 255))
				 (round (* light-intensity 255))
				 255))))))))
