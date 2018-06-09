;;(defstruct rgba r g b a)
;;(defparameter white (make-rgba :r 255 :g 255 :b 255 :a 255))
(defparameter white '(255 255 255 255))
(defmacro set-pixel (img point color)
  `(setf (pixel ,img (y ,point) (x ,point))
	 (values-list ,color)))
;;(list (r ,color)
;;	(g ,color)
;;	(b ,color)
;;	(a ,color)))))


(defun draw-line (img line &key (color white))
  (let ((points (enum-pixels-on-line line)))
    (loop for point in points do
	 (set-pixel img point color))))


(defun attach-z (points-list z0 z1)
  (loop for point in points-list
     with num-steps = (1- (length points-list))
     with curr-z = z0
     with z-step = (if (= 0 num-steps) 0 (/ (- z1 z0) num-steps))
     do
       (setf (z point) curr-z)
       (incf curr-z z-step)))


(defun draw-horizontal-line (render-img line
			     &key
			       (color white))
  (multiple-value-bind (v0 v1)
      (if (< (>> line end-1 x) (>> line end-2 x))
	  (values (end-1 line) (end-2 line))
	  (values (end-2 line) (end-1 line)))
    (let* ((curr-z (z v0))
	   (x-step (- (x v1) (x v0)))
	   (z-step (if (= x-step 0)
		       0
		       (/ (- (z v1) (z v0))
			  x-step))))
      (loop for x from (x v0) to (x v1) do
	   (cond
	     ((null (z-buffer render-img))
	      (set-pixel (img render-img) (make-v :x x :y (y v0)) color))
	     ((< curr-z (aref (z-buffer render-img) x (y v0)))
	      (set-pixel (img render-img) (make-v :x x :y (y v0)) color)
	      (setf (aref (z-buffer render-img) x (y v0)) curr-z)))
	   (incf curr-z z-step)))))


(defun draw-shaded-triangle (img triangle
			     &key
			       (color white))
  (let ((horizontal-lines (enum-horizontal-lines-on-triangle triangle)))
    (loop for y being the hash-keys in horizontal-lines do
	 (let ((line (gethash y horizontal-lines)))
	   (draw-horizontal-line img
				 line
				 :color color)))))


(defun enum-horizontal-lines-on-triangle (triangle)
  (loop for side in (get-sides triangle)
     with horizontal-lines = (make-hash-table :test 'eq)
     do
       (loop 
	  with points = (enum-pixels-on-line side)
	  initially (attach-z points (>> side end-1 z) (>> side end-2 z))
	  for point in points
	  do
	    (let ((line (gethash (y point) horizontal-lines)))
	      (cond
		((null line)
		 (setf (gethash (y point) horizontal-lines)
		       (make-line :end-1 point)))
		((null (end-2 line))
		 (setf (end-2 line) point)
		 (when (< (x point) (>> line end-1 x))
		   (setf (gethash (y point) horizontal-lines)
			 (flip-line line))))
		((< (x point) (>> line end-1 x))
		 (setf (end-1 line) point))
		((> (x point) (>> line end-2 x))
		 (setf (end-2 line) point)))))
     finally (return horizontal-lines)))


(defun enum-pixels-on-line (line)
  (let ((point-list nil))
    (multiple-value-bind (x0 y0 x1 y1 dx dy)
	(if (steep-p line)
	    (values (y0 line) (x0 line) (y1 line) (x1 line)
		    (get-delta line 'y) (get-delta line 'x))
	    (values (x0 line) (y0 line) (x1 line) (y1 line)
		    (get-delta line 'x) (get-delta line 'y)))
      (multiple-value-bind (first-x first-y second-x second-y final-dx final-dy)
	  (if (< x0 x1)
	      (values x0 y0 x1 y1 dx dy)
	      (values x1 y1 x0 y0 (neg dx) (neg dy)))
	(let ((y first-y)
	      (err 0))
	  (loop for x from first-x upto second-x 
	     do 
	       (setf err (+ err final-dy))
	       (if (steep-p line)
		   (setf point-list (cons (make-v :x y :y x) point-list))
		   (setf point-list (cons (make-v :x x :y y) point-list)))
	       (when (>= (abs err) final-dx)
		 (if (>= final-dy 0)
		     (progn
		       (setf y (1+ y))
		       (setf err (- err final-dx)))
		     (progn
		       (setf y (1- y))
		       (setf err (+ err final-dx)))))))))
    (nreverse point-list)))

(defun rasterize (triangle scale)
  (list->triangle
   (loop for vertex in (vertices triangle)
      collect
	(let ((x (clamp (round (* (11to01 (x vertex)) scale))
			0
			(1- scale)))
	      (y (clamp (round (* (11to01 (y vertex)) scale))
			0
			(1- scale)))
	      (z (z vertex)))
	  (make-v :x x :y y :z z)))))


(defun wire-render (model img projection
		    &key
		      (color white))
  (loop for triangle in (triangles model) do
       (let* ((projected-triangle (project triangle projection))
	      (rasterized-triangle (rasterize projected-triangle
					      (first (resolution img)))))
	 (loop for side in (sides rasterized-triangle) do
	      (draw-line img side :color color)))))


(defun get-triangle-light-intensity (triangle light-src)
  (dot (get-triangle-normal triangle)
       light-src))


(defun triangle-render (model img projection &key (light-src nil))
  (unless light-src
    (setf light-src (third projection)))
  (loop for triangle in (triangles model) do
       (let ((light-intensity (get-triangle-light-intensity
			       triangle
			       light-src)))
	 (when (> light-intensity 0)
	   (let* ((projected-triangle (project triangle projection))
		  (rasterized-triangle (rasterize projected-triangle
						  (first (resolution img)))))
	     (draw-shaded-triangle
	      img
	      rasterized-triangle
	      :color (list (round (* light-intensity 255))
			   (round (* light-intensity 255))
			   (round (* light-intensity 255))
			   255)))))))
