(asdf:defsystem :3d-render
  :name "3d-render"
  :depends-on (opticl opticl/test cl-ppcre)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "linalg")
   (:file "loader")
   (:file "3d-render")
   (:file "tests")))
