;;;; source.lisp --- A source for memory-related events in SBCL.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defclass source ()
  ())

(defmethod record:setup ((source source) (sink t))
  (values))

(defmethod record:start ((source source) (sink t))
  (record:add-chunk source sink (list (make-event :initial-memory)))
  (sb-int:encapsulate 'sb-kernel:sub-gc 'gc-event
                      (a:curry #'instrumented-sub-gc source sink)))

(defmethod record:stop ((source source) (sink t))
  (sb-int:unencapsulate 'sb-kernel:sub-gc 'gc-event)
  (record:add-chunk source sink (list (make-event :final-memory))))

(defmethod record:teardown ((source source) (sink t))
  (values))
