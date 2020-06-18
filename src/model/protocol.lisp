;;;; protocol.lisp --- Protocol functions provided by model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; Name protocol

(defgeneric name (object))

;;; Temporal interval protocol

(defgeneric start-time (interval))

(defgeneric end-time (interval))

(defgeneric duration (interval))

;;; Default behavior

(defmethod duration ((interval t))
  (- (end-time interval) (start-time interval)))

;;;

(defgeneric finished? (interval))

(defgeneric end-time* (interval))

(defgeneric duration* (interval))

;;; Default behavior

(defmethod finished? ((interval t))
  (not (null (end-time interval))))

(defmethod end-time* ((interval t))
  (or (end-time interval) (/ (get-internal-real-time) internal-time-units-per-second)))

(defmethod duration* ((interval t))
  (- (end-time* interval) (start-time interval)))

;;; Run protocol
;;;
;;; Extends temporal interval protocol
;;;
;;; The run, trace and node protocols form the raw view onto collected
;;; data: a run collects traces at certain points in time. Each trace
;;; consists of a list of samples corresponding to the stack of active
;;; function calls in a particular thread.

(defgeneric map-threads (function run)
  (:documentation
   "Call FUNCTION with each thread information object stored in RUN."))

(defgeneric threads (run)
  (:documentation
   "Return a sequence of all thread information objects stored in RUN."))

(defgeneric map-functions (function run)
  (:documentation
   "Call FUNCTION with each function information object stored in RUN."))

(defgeneric functions (run)
  (:documentation
   "Return a sequence of all function information objects stored in RUN."))

(defgeneric map-traces (function run)
  (:documentation
   "Call FUNCTION with each trace stored in RUN."))

(defgeneric traces (run)
  (:documentation
   "Return a sequence of all traces stored in RUN."))

;;; Trace protocol

(defgeneric thread (trace)
  (:documentation
   "Return the thread in which TRACE was acquired."))

(defgeneric time (trace)
  (:documentation
   "Return the time at which TRACE was acquired."))

(defgeneric map-samples (function trace)
  (:documentation
   "Call FUNCTION for each sample in TRACE.

The lambda-list of FUNCTION must be compatible to

  (sample)"))

;;; Sample protocol
;;;
;;; Extends name protocol.


;;; Node protocol
;;;
;;; Extends name protocol.
;;;
;;; The node protocol facilities viewing the collected data as a tree
;;; in which parents are callers and children are callees.

(defgeneric hit-count (node)
  (:documentation
   "Return the number of times NODE appeared active in the run."))

(defgeneric children (node))

(defmethod children ((node t))
  '())

(defmethod depth ((node t))
  (if-let ((children (children node)))
    (1+ (reduce #'max children :key #'depth))
    0))

;;; Tree protocol

(defgeneric map-threads (function tree)) ; TODO clashes with run protocol
