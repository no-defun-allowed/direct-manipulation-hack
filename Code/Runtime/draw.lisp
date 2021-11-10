(cl:defpackage :direct-manipulation-hack-drawing
  (:use :cl)
  (:local-nicknames (#:d #:direct-manipulation-hack)
                    (#:v #:3d-vectors))
  (:export #:draw #:draw-block #:draw-line #:clear-scene #:trail #:add-to-trail))
(cl:in-package :direct-manipulation-hack-drawing)

(defgeneric draw (thing))

(defun draw-block (position block-id &optional (metadata 0))
  (let ((absolute-position (v:vapply (v:v+ d::*origin* position)
                                     round)))
    (mcpi:set-block-data
     block-id
     d::*connection*
     (v:vx absolute-position)
     (v:vy absolute-position)
     (v:vz absolute-position)
     metadata)))

(defun draw-line (position1 position2 block-id)
  (loop for n from 0.0 to 1.0 by 0.1
        do (draw-block (v:vlerp position1 position2 n) block-id)))

(defun clear-scene ()
  (multiple-value-bind (x1 x2 z1 z2)
      (d::plot-bounds d::*plot*)
    (setf (mcpi:blocks d::*connection*
                       x1 (+ d::+floor+ 1)  z1
                       x2 (+ d::+floor+ 32) z2)
          0)))

(defstruct ring-buffer
  vector position)

(defun ring-buffer (size)
  (make-ring-buffer
   :vector (make-array size :initial-element nil)
   :position 0))

(defun add-to-buffer (position buffer)
  (setf (svref (ring-buffer-vector buffer)
               (ring-buffer-position buffer))
        position
        (ring-buffer-position buffer)
        (mod (1+ (ring-buffer-position buffer))
             (length (ring-buffer-vector buffer)))))

(defun draw-buffer (buffer block-id)
  (loop for position across (ring-buffer-vector buffer)
        unless (null position)
          do (draw-block position block-id)))

(defstruct trail
  ring-buffer
  block-id)

(defun trail (length block-id)
  (make-trail
   :ring-buffer (ring-buffer length)
   :block-id block-id))

(defun add-to-trail (position trail)
  (add-to-buffer position (trail-ring-buffer trail)))

(defmethod draw ((trail trail))
  (draw-buffer (trail-ring-buffer trail)
               (trail-block-id trail)))
