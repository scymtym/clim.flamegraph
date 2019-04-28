(cl:in-package #:clim.flamegraph.model)

;;; `standard-run'

(defclass standard-run (temporal-interval-mixin
                        print-items:print-items-mixin)
  ((%traces  :reader  traces
             :writer  (setf %traces))
   (%threads :reader  threads
             :writer  (setf %threads))))

(defun threads-in-traces (traces)
  (let ((threads (make-hash-table :test #'eq)))
    (map nil (lambda (trace)
               (ensure-gethash (thread trace) threads t))
         traces)
    (coerce (hash-table-keys threads) 'simple-vector)))

(defmethod shared-initialize :after ((instance   standard-run)
                                     (slot-names t)
                                     &key
                                     (traces  nil traces-supplied?)
                                     (threads nil threads-supplied?))
  (when traces-supplied?
    (setf (%traces instance) traces))
  (cond (threads-supplied?
         (setf (%threads instance) threads))
        (traces-supplied?
         (setf (%threads instance) (threads-in-traces traces)))))

(defmethod print-items:print-items append ((object standard-run))
  `((:thread-count ,(length (threads object)) " ~:D thread~:P" ((:after :duration)))
    (:trace-count  ,(length (traces object))  " ~:D trace~:P"  ((:after :thread-count)))))

;;; `standard-trace'

(defclass standard-trace (temporal-point-mixin
                          print-items:print-items-mixin)
  ((%thread  :initarg :thread
             :reader  thread)
   (%samples :initarg :samples
             :reader  samples)))

(defmethod map-samples ((function function) (trace standard-trace))
  (map nil function (samples trace)))

(defmethod print-items:print-items append ((object standard-trace))
  `((:sample-count ,(length (samples object)) "~:D sample~:P")))

;;; `standard-thread'

(defclass standard-thread (name-mixin)
  ())

;;; `standard-sample'

(defclass standard-sample (name-mixin)
  ())
