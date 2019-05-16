(cl:in-package #:clim.flamegraph.model)

(defclass qualified-name (name-mixin)
  ((%container :initarg :container
               :reader  container)))
