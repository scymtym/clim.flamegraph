;;;; standard-run.lisp --- Default implementations of run, trace, etc. protocols
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `standard-run'

(defclass standard-run (temporal-interval-mixin
                        print-items:print-items-mixin)
  ((%threads   :reader  threads
               :writer  (setf %threads))
   (%functions :reader  functions
               :writer  (setf %functions))
   (%traces    :reader  traces
               :writer  (setf %traces))))

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
               (when (compute-applicable-methods #'thread (list trace))
                 (ensure-gethash (thread trace) threads t)))
         traces)
    (coerce (hash-table-keys threads) 'simple-vector)))

(defmethod register-function ((node name-mixin) (functions hash-table))
  (let* ((name     (if (typep (name node) 'standard-function)
                       (name (name node))
                       (name node)))
         (function (ensure-gethash name functions
                                   (make-instance 'standard-function :name name))))
    (setf (slot-value node '%name) function) ; TODO HACK
    function))

(defun functions-in-traces (traces)
  (let ((functions (make-hash-table :test #'equal)))
    (map nil (rcurry #'register-functions functions) traces)
    (coerce (hash-table-values functions) 'simple-vector))) ; TODO keeping the hash-table would be useful

(defmethod shared-initialize :after ((instance   standard-run)
                                     (slot-names t)
                                     &key
                                     start-time
                                     end-time
                                     (threads   nil threads-supplied?)
                                     (functions nil functions-supplied?)
                                     (traces    nil traces-supplied?))
  (when traces-supplied?
    (setf (%traces instance) traces))

  ;; Initialize thread list either from the THREADS initarg or by
  ;; collecting all threads in TRACES.
  (cond (threads-supplied?
         (setf (%threads instance) threads))
        (traces-supplied?
         (setf (%threads instance) (threads-in-traces traces))))

  ;; Initialize function statistics either from the FUNCTIONS initarg
  ;; or by collecting all FUNCTIONS in TRACES.
  (cond (functions-supplied?
         (setf (%functions instance) functions))
        (traces-supplied?
         (setf (%functions instance) (functions-in-traces traces))))

  ;; Initialize start and end time either from the START-TIME and
  ;; END-TIME initargs or by computing the temporal bounds of TRACES.
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

(defmethod register-functions ((node standard-trace) (functions hash-table))
  (let ((thread (thread node)))
    (map-samples (lambda (sample)
                   (let ((function (register-function sample functions)))
                     (pushnew thread (calling-threads function) :test #'eq)
                     (incf (hit-count function))))
                 node)))

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
