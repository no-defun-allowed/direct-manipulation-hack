(in-package :direct-manipulation-hack)

(defconstant +plot-size+ 32)
(defconstant +usable-space+ 24)
(defconstant +plot-area-size+ 8)
(defconstant +floor+ 4)

(defvar *plots* (make-hash-table :test #'equal))
(defvar *connection* (make-instance 'mcpi:connection))
(defvar *plot-lock* (bt:make-lock "Plot lock"))
(defmacro with-plot-lock (() &body body)
  `(bt:with-lock-held (*plot-lock*)
     ,@body))

(defun claim-plot (x z plot)
  (multiple-value-bind (data claimed?)
      (gethash (cons x z) *plots*)
    (declare (ignore data))
    (if claimed?
        (error "Already claimed")
        (setf (gethash (cons x z) *plots*) plot))))

(defclass plot ()
  ((%name :initarg :name :initform "" :accessor name)
   (%parent :initarg :parent :reader parent)
   (%children :initform '() :reader children)
   (%package :reader plot-package)
   (%program :initarg :program :accessor program)
   (%x :initarg :x :reader x)
   (%z :initarg :z :reader z)))

(defun fresh-package (x z)
  (make-package (format nil "PLOT-~d-~d" x z)
                :use '(:cl
                       :3d-vectors
                       :direct-manipulation-hack
                       :direct-manipulation-hack-drawing
                       :direct-manipulation-hack-collection)))

(defun plot-bounds (plot)
  (let ((x (x plot))
        (z (z plot)))
    (values     (* x      +plot-size+)
            (1- (* (1+ x) +plot-size+))
                (* z      +plot-size+)
            (1- (* (1+ z) +plot-size+)))))

(defun draw-new-plot (plot)
  (with-accessors ((x x) (z z)) plot
    (multiple-value-bind (x1 x2 z1 z2)
        (plot-bounds plot)
      (setf (mcpi:blocks *connection* x1 +floor+ z1 x2 +floor+ z2)
            (if (evenp (logxor x z)) 24 1)))))

(defun run-in-plot (plot program)
  (let ((*package* (plot-package plot)))
    (load (make-string-input-stream program))))

(defmethod (setf program) :after (program plot)
  (run-in-plot plot program))

(defmethod initialize-instance :after ((plot plot) &key (prototypical-plot-p nil))
  (claim-plot (x plot) (z plot) plot)
  (draw-new-plot plot)
  (setf (slot-value plot '%package)
        (fresh-package (x plot) (z plot)))
  (if prototypical-plot-p
      (setf (slot-value plot '%parent) plot)
      (push plot (slot-value (parent plot) '%children)))
  ;; Run program
  (setf (program plot) (program plot)))

(defun clone-plot (parent x z)
  (make-instance 'plot
                 :parent parent
                 :name (name parent)
                 :x x
                 :z z
                 :program (program parent)))
