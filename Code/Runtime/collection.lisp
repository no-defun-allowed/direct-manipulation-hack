(cl:defpackage :direct-manipulation-hack-collection
  (:use :cl)
  (:export #:collection #:in-collection
           #:do-collection #:map-collection))
(in-package :direct-manipulation-hack-collection)

(defstruct collection
  (elements '()))
(defun collection ()
  (make-collection))

(defun in-collection (collection element)
  (push element (collection-elements collection))
  element)

(defmacro do-collection ((element collection) &body body)
  `(dolist (,element (collection-elements ,collection))
     ,@body))
(defun map-collection (function collection)
  (map 'nil function (collection-elements collection))
  collection)
