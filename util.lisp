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


(defun sqr (x) (* x x))


