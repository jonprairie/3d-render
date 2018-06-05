(defstruct (v :conc-name) x y z)
(defstruct (line :conc-name) end-1 end-2)
(defstruct (triangle :conc-name) v0 v1 v2)

(defun v- (v0 v1)
  (make-v :x (- (x v0) (x v1))
	  :y (- (y v0) (y v1))
	  :z (- (z v0) (z v1))))

(defun v/ (v s)
  (make-v :x (/ (x v) s)
	  :y (/ (y v) s)
	  :z (/ (z v) s)))

(defun sides (triangle)
  (list (make-line :end-1 (v0 triangle) :end-2 (v1 triangle))
	(make-line :end-1 (v1 triangle) :end-2 (v2 triangle))
	(make-line :end-1 (v2 triangle) :end-2 (v0 triangle))))

(defun vertices (triangle)
  (list (v0 triangle)
	(v1 triangle)
	(v2 triangle)))

(defun get-triangle-normal (triangle)
  (normalize (cross (v- (v2 triangle) (v0 triangle))
		    (v- (v1 triangle) (v0 triangle)))))

(defun zero-vectorp (v)
  (and (= (x v) 0)
       (= (y v) 0)
       (= (z v) 0)))

(defun normalize (v)
  (if (zero-vectorp v)
      v
      (let ((len (sqrt (dot v v))))
	(v/ v len))))

(defun x0 (line)
  (>> line end-1 x))
(defun y0 (line)
  (>> line end-1 y))
(defun z0 (line)
  (>> line end-1 z))
(defun x1 (line)
  (>> line end-2 x))
(defun y1 (line)
  (>> line end-2 y))
(defun z1 (line)
  (>> line end-2 z))

(defun flip-line (line)
  (let ((e1 (end-1 line))
	(e2 (end-2 line)))
    ;;would two setf's be faster?
    (make-line :end-1 e2 :end-2 e1)))


(defun get-delta (line dimension)
  (- (funcall dimension (end-2 line))
     (funcall dimension (end-1 line))))


(defun steep-p (line &key (rise 'y) (run 'x))
  (> (abs (get-delta line rise))
     (abs (get-delta line run))))


(defun get-sides (triangle)
  (list (make-line :end-1 (v0 triangle) :end-2 (v1 triangle))
	(make-line :end-1 (v1 triangle) :end-2 (v2 triangle))
	(make-line :end-1 (v2 triangle) :end-2 (v0 triangle))))


(defun line-dz (line)
  (- (>> line end-2 z) (>> line end-1 z)))


(defun 11to01 (num)
  (/ (1+ num) 2))


(defun dot (v0 v1)
  (+ (* (x v0) (x v1))
     (* (y v0) (y v1))
     (* (z v0) (z v1))))


(defun cross (v0 v1)
  (make-v :x (- (* (y v0) (z v1))
		(* (z v0) (y v1)))
	  :y (- (* (z v0) (x v1))
		(* (x v0) (z v1)))
	  :z (- (* (x v0) (y v1))
		(* (y v0) (x v1)))))


(defun list->triangle (vertex-list)
  (make-triangle :v0 (first vertex-list)
		 :v1 (second vertex-list)
		 :v2 (third vertex-list)))


(defun project (triangle projection)
  (list->triangle
   (loop for vertex in (vertices triangle)
      collect (make-v :x (dot vertex (first projection))
		      :y (dot vertex (second projection))
		      :z (dot vertex (third projection))))))
