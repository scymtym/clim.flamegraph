(cl:in-package #:clim.flamegraph.model)

(defclass standard-event (name-mixin
                          temporal-point-mixin
                          print-items:print-items-mixin)
  ((%properties :initarg :properties
                :reader  properties)))

(defmethod time :around ((object standard-event))
  (/ (call-next-method) 1000000))

(defmethod thread ((thing standard-event))
  :global)
