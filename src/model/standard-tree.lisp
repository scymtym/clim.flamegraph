;;;; standard-tree.lisp --- Default implementations of the tree and node protocols
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `standard-tree'

(defclass standard-tree ()
  ())

;;; `standard-node'

(defclass standard-node (name-mixin)
  ((%children  :initarg  :children
               :reader   %children
               :initform (make-hash-table :test #'equal)) ; TODO start with list
   (%hit-count :initarg  :hit-count
               :reader   hit-count
               :accessor hit-count
               :initform 0)))

(defmethod find-child ((name t) (node standard-node))
  (gethash name (%children node)))

(defmethod (setf find-child) ((new-value t) (name t) (node standard-node))
  (setf (gethash name (%children node)) new-value))

(defmethod ensure-child ((name t) (node standard-node) (thunk function))
  (ensure-gethash name (%children node) (funcall thunk)))
