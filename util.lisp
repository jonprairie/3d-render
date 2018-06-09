(defmacro let-cond (&body body)
  (when (car body)
    `(let ((result ,(caar body)))
       (if result
	   (progn
	     ,@(cdr (car body)))
	   (let-cond
	     ,@(cdr body))))))


(defun neg (x)
  (- 0 x))


(defun parse-float (str)
  (with-input-from-string (stream str)
    (read stream)))


(defun clamp (n min max)
  (cond
    ((<= min n max) n)
    ((< n min) min)
    (t max)))


(defun sqr (x) (* x x))


(defmacro >> (obj &rest attributes)
  (if (null (first attributes))
       obj
       `(>> (,(first attributes) ,obj)
	    ,(values-list (rest attributes)))))
