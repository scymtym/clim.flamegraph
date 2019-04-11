(cl:in-package #:clim.flamegraph.model)

;;; `standard-tree'

(defclass standard-tree ()
  ())

;;; `standard-node'

(defclass standard-node (name-mixin)
  ((%children  :initarg  :children
               :reader   children
               :accessor children)
   (%hit-count :initarg  :hit-count
               :reader   hit-count
               :accessor hit-count
               :initform 0)))
