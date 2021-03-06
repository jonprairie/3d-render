(defparameter test-img nil)

(defparameter straight-on
  (list
   (make-v :x 1 :y 0 :z 0)
   (make-v :x 0 :y -1 :z 0) ; origin is in top left for some reason
   (make-v :x 0 :y 0 :z -1)))
(defparameter right-side
  (list
   (make-v :x 0 :y 0 :z 1)
   (make-v :x 0 :y -1 :z 0) ; origin is in top left for some reason
   (make-v :x -1 :y 0 :z 0)))
(defparameter back-side
  (list
   (make-v :x -1 :y 0 :z 0)
   (make-v :x 0 :y -1 :z 0) ; origin is in top left for some reason
   (make-v :x 0 :y 0 :z 1)))
(defparameter bottom
  (list
   (make-v :x 1 :y 0 :z 0)
   (make-v :x 0 :y 0 :z -1) ; origin is in top left for some reason
   (make-v :x 0 :y 1 :z 0)))
;;need to add z-components!
;;(defparameter right-side
;;  '((0 0 1)
;;    (0 1 0)))
;;(defparameter left-side
;;  '((0 0 -1)
;;    (0 1 0)))
;;(defparameter angled-pi/4
;;  '((.707 0 -.707)
;;    (0 1 0)))
;;(defparameter angled-3pi/4
;;  '((.707 0 -.707)
;;    (0 1 0)))
;;(defparameter stupid
;;  '((.707 0 -.707)
;;    (-.577 .577 -.577)))
;;(defparameter bottom-stupid
;;  '((.707 -.707 0)
;;    (0 0 1)))


(defun reset-z-buffer (img)
  (setf (z-buffer img)
	(make-array (resolution img)
		    :initial-element most-positive-double-float)))


(defun reset-test-img (&key (resolution '(32 32)) (color '(255 255 255 255)))
  (setf test-img
	(make-render-img
	 :img (opticl::make-8-bit-rgba-image
	       (first resolution)
	       (second resolution))
	 :resolution resolution
	 :z-buffer (make-array resolution
			       :initial-element most-positive-double-float)))
  (let ((out-img (output-image "~/test.png")))
    (apply #'fill-image (cons (img test-img) color))
    (write-png-file out-img (img test-img))
    test-img))


(defun refresh-image ()
  (let ((out (output-image "~/test.png")))
    (write-png-file out (img test-img))))


;;(defun test-draw-line () 
;;  (reset-test-img)
;;  (let* ((out (output-image "~/test.png")) 
;;	 (inp (read-png-file out)))
;;    (setf test-img inp)
;;    (time
;;     (draw-line test-img '(16 8) '(0 0)))
;;    (write-png-file out test-img)))


;;make models in these render-test functions!

;;(defun test-wireframe-render ()
;;  (reset-test-img :resolution '(1024 1024) :color '(255 255 255 255))
;;  (multiple-value-bind (vertices faces scale)
;;      (time
;;       ;;(read-obj-file "~/quicklisp/local-projects/3d-render/obj/mini-cooper.obj"))
;;       (read-obj-file "~/quicklisp/local-projects/3d-render/obj/head.obj"))
;;    (time
;;     (wire-render vertices faces test-img
;;		  ;;:projection straight-on
;;		  ;;:projection right-side
;;		  :projection stupid
;;		  ;;:projection angled-pi/4
;;		  :color color
;;		  :scale scale)))
;;  (refresh-image))


(defparameter base-path "~/quicklisp/local-projects/3d-render/obj/")
(defparameter obj-cache nil)
(defun test-triangle-render (&key
			       (use-cache nil)
			       (resolution '(1024 1024))
			       (file-name "head.obj")
			       (projection straight-on))
  (reset-test-img :resolution resolution :color '(0 0 0 255))
  (let ((model (time (if (and use-cache obj-cache)
			 obj-cache
			 (let* ((full-path (concatenate 'string
							base-path
							file-name))
				(obj (read-obj-file full-path)))
			   (setf obj-cache obj))))))
    (time
     (triangle-render model test-img projection)))
  (refresh-image))
