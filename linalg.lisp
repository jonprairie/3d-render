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


(defun normalize (v)
  (if (and (= (first v) 0)
	   (= (second v) 0)
	   (= (third v) 0))
      v
      (let ((len (sqrt (apply #'+ (mapcar #'sqr v)))))
	(mapcar #'(lambda (x) (/ x len)) v))))


