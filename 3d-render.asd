(asdf:defsystem :3d-render
  :name "3d-render"
  :serial t
  :depends-on (opticl opticl/test cl-ppcre)
  :components
  ((:file "package")
   (:file "3d-render")))
