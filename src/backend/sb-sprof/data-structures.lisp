(cl:in-package #:clim.flamegraph.backend.sb-sprof)

(defun make-sample (info)
  (make-instance 'model::standard-sample
                 :name (sb-sprof::node-name
                        (sb-sprof::make-node info))))

(defun make-trace (thread time trace)
  (let ((samples '()))
    (sb-sprof:map-trace-samples
     (lambda (info pc-offset)
       (declare (ignore pc-offset))
       (push (make-sample info) samples))
     trace)
    (make-instance 'model::standard-trace
                   :thread  thread
                   :time    time
                   :samples samples)))

(defun make-traces (samples)
  (let ((threads (make-hash-table :test #'eq))
        (traces  '()))
    (sb-sprof:map-traces
     (lambda (thread time trace)
       (let ((thread (ensure-gethash
                      thread threads
                      (make-instance 'model::standard-thread
                                     :name (sb-thread:thread-name thread)
                                     :native-thread thread))))
         (push (make-trace thread time trace) traces)))
     samples)
    traces))  ; TODO return threads from here; no need gather again in standard-run.lisp

(defun make-run (&optional (samples sb-sprof::*samples*))
  (let ((traces (make-traces samples)))
    (make-instance 'model::standard-run :start-time (/ (sb-sprof::samples-start-time samples) internal-time-units-per-second)
                                        :end-time   (when-let ((end (sb-sprof::samples-end-time samples)))
                                                      (/ end internal-time-units-per-second))
                                        :traces     traces)))
(defparameter *run* (cl:time (make-run)))
