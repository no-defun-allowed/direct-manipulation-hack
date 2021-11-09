(cl:in-package :direct-manipulation-hack)

;; Bootstrap the prototypical plot
(defvar *prototypical-plot*
  (make-instance 'plot
                 :name "Prototypical plot"
                 :prototypical-plot-p t
                 :x 0
                 :z 0
                 :program (alexandria:read-file-into-string
                           (asdf:system-relative-pathname
                            :direct-manipulation-hack-examples
                            "lissajous.lisp"))))

(let ((plot (clone-plot *prototypical-plot* 0 1)))
  (setf (program plot)
        (alexandria:read-file-into-string
         (asdf:system-relative-pathname
          :direct-manipulation-hack-examples
          "pendulum.lisp"))
        (name plot) "Double pendulum"))
