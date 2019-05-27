(cl:in-package #:clim.flamegraph.backend.advice)

(defclass source ()
  ((%specification :initarg :specification
                   :reader  specification)
   ;; sampling (i.e. only collect every nth event)
   ;; depth-limit
   ;; duration-limit
   ;;
   (%run           :accessor run)
   (%thread        :accessor thread)))

(defmethod recording:setup ((source source) (run t))
  (map nil #'record (specification source))
  (setf *recording-state* (make-recording-state))
  (setf (run    source) run
        (thread source) (bt:make-thread (curry #'work run source)
                                        :name "advice source worker")))

(defmethod recording:start ((source source) (run t)) ; TODO store state in the source?
  (setf (recording-state-recording? *recording-state*) t))

(defmethod recording:stop ((source source) (run t))
  (setf (recording-state-recording? *recording-state*) nil))

(defmethod recording:teardown ((source source) (run t))
  (setf *recording-state* nil)
  (bt:join-thread (thread source)))
