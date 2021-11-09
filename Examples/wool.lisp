(defun tick ()
  (draw-block (vec 0 0 0) 35 (mod (round *t* 1/4) 16)))
