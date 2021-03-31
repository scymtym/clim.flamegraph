(cl:in-package #:clim.flamegraph.view.timeline)

;;; `timeline'

(defclass timeline (model::temporal-interval-mixin)
  ((%lanes :initarg :lanes
           :reader  lanes)))

;;; `lane'

(defclass lane ()
  ((%description :initarg  :description
                 :reader   description)
   (%elements    :initarg  :elements
                 :type     list         ; TODO vector?
                 :accessor elements
                 :initform '())))

(defmethod model::depth ((node lane))
  (reduce #'max (elements node) :key #'model::depth :initial-value 0))
