;;;; standard-event.lisp --- Representation of events.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

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
