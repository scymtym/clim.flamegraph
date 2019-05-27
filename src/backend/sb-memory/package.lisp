(cl:defpackage #:clim.flamegraph.backend.sb-memory
  (:use
   #:cl)

  (:local-nicknames
   (#:time   #:clim.flamegraph.time)
   (#:model  #:clim.flamegraph.model)

   (#:record #:clim.flamegraph.recording)))

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defclass source ()
  ())

(defmethod record:setup ((source source) (run t))
  )

(defmethod record:start ((source source) (run t))
  (setf sb-ext:*after-gc-hooks* (list (lambda ()
                                        (clim.flamegraph.recording:add-chunk
                                         source run (list (make-instance 'model::standard-event
                                                                         :name       :gc
                                                                         :time       (time:real-time)
                                                                         :properties (list :used (sb-vm::dynamic-usage)))))))))

(defmethod record:stop ((source source) (run t))
  (setf sb-ext:*after-gc-hooks* '()))

(defmethod record:teardown ((source source) (run t))
  )
;; sb-ext:*gc-run-time*

()
