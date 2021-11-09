(defun coefficient (scale freq off)
  (* scale (sin (* 2 pi (+ off (* *t* freq))))))

(defvar *scene* (trail 100 22))

(defun tick ()
  (add-to-trail (vec (coefficient 8 0.1 0.0)
                     (coefficient 8 0.2 0.0)
                     (coefficient 8 0.5 0.0))
                *scene*)
  (clear-scene)
  (draw *scene*))
