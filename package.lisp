(defpackage :3d-render
  (:use #:cl #:opticl #:opticl-test #:opticl-color #:cl-ppcre)
  (:import-from :opticl
		#:pixel #:read-png-file #:write-png-file #:fill-image #:make-8-bit-gray-image)
  (:import-from :opticl-test
		#:output-image))

(in-package #:3d-render)
