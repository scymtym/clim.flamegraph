(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defclass source ()
  ())

(defmethod record:setup ((source source) (run t))
  )

(defmethod record:start ((source source) (run t))
  (clim.flamegraph.recording:add-chunk
   source run (list (make-event :initial-memory (time:real-time) (sb-vm::dynamic-usage))))
  (sb-int:encapsulate 'sb-kernel:sub-gc 'gc-event
                      (a:curry #'instrumented-sub-gc source run)))

(defmethod record:stop ((source source) (run t))
  (sb-int:unencapsulate 'sb-kernel:sub-gc 'gc-event)
  (clim.flamegraph.recording:add-chunk
   source run (list (make-event :final-memory (time:real-time) (sb-vm::dynamic-usage)))))

(defmethod record:teardown ((source source) (run t))
  )
