;;;; standard-run.lisp --- Default implementations of run, trace, etc. protocols
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `standard-run'

(defclass standard-run (temporal-interval-mixin
                        print-items:print-items-mixin)
  ((%traces  :reader  traces
             :writer  (setf %traces))
   (%threads :reader  threads
             :writer  (setf %threads))))

(defun temporal-bounds-in-traces (traces)
  (let ((start most-positive-fixnum)
        (end   most-negative-fixnum))
    (map nil (lambda (trace)
               (cond ((typep trace 'temporal-point-mixin) ; TODO
                      (let ((time (time trace)))
                        (minf start time)
                        (maxf end   time)))
                     ((typep trace 'temporal-interval-mixin)
                      (minf start (start-time trace))
                      (maxf end   (end-time trace)))))
         traces)
    (values start end)))

(defun threads-in-traces (traces)
  (let ((threads (make-hash-table :test #'eq)))
    (map nil (lambda (trace)
               (ensure-gethash (thread trace) threads t))
         traces)
    (coerce (hash-table-keys threads) 'simple-vector)))

(defmethod shared-initialize :after ((instance   standard-run)
                                     (slot-names t)
                                     &key
                                     start-time
                                     end-time
                                     (traces    nil traces-supplied?)
                                     (threads   nil threads-supplied?))
  (when traces-supplied?
    (setf (%traces instance) traces))
  (cond (threads-supplied?
         (setf (%threads instance) threads))
        (traces-supplied?
         (setf (%threads instance) (threads-in-traces traces))))
  (when (and (or (not start-time) (not end-time))
             traces-supplied?)
    (multiple-value-bind (start end) (temporal-bounds-in-traces traces)
      (unless start-time
        (setf (slot-value instance '%start-time) start))
      (unless end-time
        (setf (end-time instance) end)))))

(defmethod print-items:print-items append ((object standard-run))
  `((:thread-count ,(length (threads object)) " ~:D thread~:P" ((:after :duration)))
    (:trace-count  ,(length (traces object))  " ~:D trace~:P"  ((:after :thread-count)))))

(defmethod map-traces ((function function) (run standard-run))
  (map nil function (traces run)))

;;; `standard-trace'

(defclass standard-trace (temporal-point-mixin
                          print-items:print-items-mixin)
  ((%thread  :initarg :thread
             :reader  thread)
   (%samples :initarg :samples
             :reader  samples)))

(defmethod time :around ((event standard-trace))
  (/ (call-next-method) 1000000))

(defmethod map-samples ((function function) (trace standard-trace))
  (map nil function (samples trace)))

(defmethod print-items:print-items append ((object standard-trace))
  `((:sample-count ,(length (samples object)) "~:D sample~:P")))

;;; `standard-thread'

(defclass standard-thread (name-mixin
                           print-items:print-items-mixin)
  ((%native-thread :initarg :native-thread
                   :reader native-thread) ; TODO temp hack
   ))

;;; `standard-sample'

(defclass standard-sample (name-mixin
                           print-items:print-items-mixin)
  ())
