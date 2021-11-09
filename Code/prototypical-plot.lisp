(cl:in-package :direct-manipulation-hack)

(defun example (name)
  (alexandria:read-file-into-string
   (asdf:system-relative-pathname
    :direct-manipulation-hack-examples
    name)))

;; Bootstrap the prototypical plot
(defvar *prototypical-plot*
  (make-instance 'plot
                 :name "Prototypical plot"
                 :prototypical-plot-p t
                 :x 0
                 :z 0
                 :program (example "wool.lisp")))

(let ((plot (clone-plot *prototypical-plot* 1 0)))
  (setf (program plot) (example "lissajous.lisp")
        (name plot) "Lissajous figure"))

(let ((plot (clone-plot *prototypical-plot* 0 1)))
  (setf (program plot) (example "pendulum.lisp")
        (name plot) "Double pendulum"))
