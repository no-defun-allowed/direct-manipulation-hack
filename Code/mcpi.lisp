(cl:defpackage :mcpi
  (:use :cl)
  (:shadow #:stream #:block #:position)
  (:export #:connection #:block #:blocks #:set-block-data #:position #:player-id))
(cl:in-package :mcpi)

(defclass connection ()
  ((%socket :reader socket)
   (%stream :reader stream)))

(defmethod initialize-instance :after ((connection connection)
                                       &key (host "localhost")
                                            (port 4711))
  (setf (slot-value connection '%socket)
        (usocket:socket-connect host port)
        (slot-value connection '%stream)
        (usocket:socket-stream (socket connection))))

(defmacro define-update (name api-name (new-value &rest arguments)
                         &key (wire-order (append arguments (list new-value))))
  `(defun ,name (,new-value connection ,@arguments)
     (write-line (format nil "~a(~{~a~^,~})"
                         ,api-name
                         (list ,@wire-order))
                 (stream connection))
     (finish-output (stream connection))))

(defmacro define-query (name api-name arguments &key (transformer '#'identity))
  `(defun ,name (connection ,@arguments)
     (let ((stream (stream connection)))
       (write-line (format nil "~a(~{~a~^,~})"
                           ,api-name
                           (list ,@arguments))
                   stream)
       (finish-output stream)
       (funcall ,transformer (read-line stream)))))

(defun parse-integer* (string)
  "Remote the second value from PARSE-INTEGER"
  (if (string= string "Fail")
      (error "No bueno")
      (values (parse-integer string))))

(define-update (setf block) "world.setBlock" (block-id x y z))
(define-update set-block-data "world.setBlock" (block-id x y z metadata)
               :wire-order (x y z block-id metadata))
(define-update (setf blocks) "world.setBlocks" (block-id x1 y1 z1 x2 y2 z2))
(define-query block "world.getBlock" (x y z) :transformer #'parse-integer*)

(defun parse-position (s)
  (apply #'3d-vectors:vec3
         (mapcar #'parse-float:parse-float
                 (split-sequence:split-sequence #\, s))))
(defun unparse-position (position)
  (format nil "~d,~d,~d"
          (round (3d-vectors:vx position))
          (round (3d-vectors:vy position))
          (round (3d-vectors:vz position))))

(define-query player-id "world.getPlayerId" (name) :transformer #'parse-integer*)
(define-query position "entity.getPos" (player-id) :transformer #'parse-position)
(define-update (setf position) "entity.setPos" (position player-id)
               :wire-order (player-id (unparse-position position)))
