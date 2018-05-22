(defparameter test-img nil)


(defun reset-test-img () 
  (setf test-img (make-8-bit-gray-image 32 32))
  (let ((out-img (output-image "~/test.png")))
    (fill-image test-img 255)
    (write-png-file out-img test-img) test-img))


(defun test-draw-line () 
  (reset-test-img)
  (let* ((out (output-image "~/test.png")) 
	 (inp (read-png-file out)))
    (setf test-img inp)
    (time
     (draw-line test-img 0 0 32 32))
    (write-png-file out test-img)))


(defun draw-line (img x0 y0 x1 y1 &optional (color 0))
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
  (register-groups-bind (x y z w)
      ("v (-?[0-9\.]*) (-?[0-9\.]*) (-?[0-9\.]*) ?(-?[0-9\.]*)?" line)
    (list x y z (if (> (length w) 0) w "1.0"))))
