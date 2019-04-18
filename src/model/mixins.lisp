(cl:in-package #:clim.flamegraph.model)

;;; `name-mixin'

(defclass name-mixin (print-items:print-items-mixin)
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (error "Missing required :NAME initarg")))

(defmethod print-items:print-items append ((object name-mixin))
  `((:name ,(name object) "~A")))

;;; `temporal-interval-mixin'

(defclass temporal-interval-mixin (print-items:print-items-mixin)
  ((%start-time :initarg  :start-time
                :reader   start-time)
   (%end-time   :initarg  :end-time
                :accessor end-time
                :initform nil)))

(defmethod print-items:print-items append ((object temporal-interval-mixin))
  `((:duration ,(duration* object) "~/text.orders-of-magnitude:print-human-readable-duration/")))
