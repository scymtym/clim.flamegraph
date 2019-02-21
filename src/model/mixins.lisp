(cl:in-package #:clim.flamegraph.model)

(defclass name-mixin ()
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (error "Missing required :NAME initarg")))
