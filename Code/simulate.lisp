(in-package :direct-manipulation-hack)

(defvar *delta-t* 0.1)
(defvar *t*)
(defvar *plot*)
(defvar *origin*)

(defun run-plot (plot)
  (multiple-value-bind (symbol type)
      (find-symbol "TICK" (plot-package plot))
    (unless (eq type 'nil)
      (let ((*plot* plot)
            (*origin* (3d-vectors:vec3
                       (* (+ (x plot) 1/2) +plot-size+)
                       (+ +floor+ 16)
                       (* (+ (z plot) 1/2) +plot-size+))))
        (funcall symbol)))))

(defun run-simulations ()
  (let ((*t* 0.0))
    (loop
      (with-plot-lock ()
        (maphash (lambda (position plot)
                   (declare (ignore position))
                   (run-plot plot))
                 *plots*))
      (sleep *delta-t*)
      (incf *t* *delta-t*))))

(bt:make-thread #'run-simulations :name "Simulation thread")
