(defparameter test-img nil)
(defparameter obj-cache nil)

(defparameter straight-on
  '((1 0 0)
    (0 1 0)))
(defparameter right-side
  '((0 0 1)
    (0 1 0)))
(defparameter left-side
  '((0 0 -1)
    (0 1 0)))
(defparameter angled-pi/4
  '((.707 0 -.707)
    (0 1 0)))
(defparameter angled-3pi/4
  '((.707 0 -.707)
    (0 1 0)))
(defparameter stupid
  '((.707 0 -.707)
    (-.577 .577 -.577)))
(defparameter bottom-stupid
  '((.707 -.707 0)
    (0 0 1)))
(defparameter bottom
  '((1 0 0)
    (0 0 1)))


(defun reset-test-img (&key (resolution '(32 32)) (color '(255 255 255 255)))
  (setf test-img (opticl::make-8-bit-rgba-image
		  (first resolution)
		  (second resolution)))
  (let ((out-img (output-image "~/test.png")))
    (apply #'fill-image (cons test-img color))
    (write-png-file out-img test-img) test-img))


(defun refresh-image ()
  (let ((out (output-image "~/test.png")))
    (write-png-file out test-img)))


(defun test-draw-line () 
  (reset-test-img)
  (let* ((out (output-image "~/test.png")) 
	 (inp (read-png-file out)))
    (setf test-img inp)
    (time
     (draw-line test-img '(16 8) '(0 0)))
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


(defun test-triangle-render (&key
			       (use-cache nil)
			       (resolution '(1024 1024))
			       (file-name "head.obj")
			       (projection straight-on))
  (reset-test-img :resolution resolution :color '(0 0 0 255))
  (destructuring-bind (vertices faces scale)
      (time (if (and use-cache obj-cache)
		obj-cache
		(let* ((base-path "~/quicklisp/local-projects/3d-render/obj/")
		       (full-path (concatenate 'string base-path file-name))
		       (obj (read-obj-file full-path)))
		  (setf obj-cache obj))))
    (time
     (triangle-render vertices faces test-img
		      :resolution resolution
		      :projection projection
		      :scale scale)))
  (refresh-image))
