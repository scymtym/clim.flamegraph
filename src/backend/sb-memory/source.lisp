;;;; source.lisp --- A source for memory-related events in SBCL.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defclass source ()
  ())

(defmethod record:setup ((source source) (run t))
  )

(defmethod record:start ((source source) (run t))
  (record:add-chunk
   source run (list (make-event :initial-memory (time:real-time) (sb-vm::dynamic-usage))))
  (sb-int:encapsulate 'sb-kernel:sub-gc 'gc-event
                      (a:curry #'instrumented-sub-gc source run)))

(defmethod record:stop ((source source) (run t))
  (sb-int:unencapsulate 'sb-kernel:sub-gc 'gc-event)
  (record:add-chunk
   source run (list (make-event :final-memory (time:real-time) (sb-vm::dynamic-usage)))))

(defmethod record:teardown ((source source) (run t))
  )
