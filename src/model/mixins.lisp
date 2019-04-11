(cl:in-package #:clim.flamegraph.model)

;;; `name-mixin'

(defclass name-mixin ()
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (error "Missing required :NAME initarg")))

;;; `temporal-interval-mixin'

(defclass temporal-interval-mixin ()
  ((%start-time :initarg  :start-time
                :reader   start-time)
   (%end-time   :initarg  :end-time
                :accessor end-time
                :initform nil)))
