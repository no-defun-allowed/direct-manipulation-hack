(asdf:defsystem :direct-manipulation-hack
  :depends-on (:usocket :parse-float :3d-vectors
               :trivial-package-local-nicknames
               :cl-who :hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "mcpi")
               (:file "plots")
               (:file "simulate")
               (:module "Runtime"
                :components ((:file "draw")
                             (:file "collection")))
               (:file "prototypical-plot")
               (:file "web")))
