(cl:in-package #:clim.flamegraph.model)

;;; Name protocol

(defgeneric name (object))

;;; Temporal interval protocol

(defgeneric finished? (interval))

(defgeneric start-time (interval))

(defgeneric end-time (interval))

(defgeneric duration (interval))

;;; Default behavior

(defmethod finished? ((interval t))
  (not (null (end-time interval))))

(defmethod duration ((interval t))
  (- (end-time interval) (start-time interval)))

;;; Run protocol
;;;
;;; Extends temporal interval protocol
;;;
;;; The run, trace and node protocols form the raw view onto collected
;;; data: a run collects traces at certain points in time. Each trace
;;; consists of a list of samples corresponding to the stack of active
;;; function calls in a particular thread.

(defgeneric map-threads (function run))

(defgeneric map-traces (function run))

;;; Trace protocol

(defgeneric thread (trace)
  (:documentation
   "Return the thread in which TRACE was acquired."))

(defgeneric time (trace)
  (:documentation
   "Return the time at which TRACE was acquired."))

(defgeneric map-samples (function trace))

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

;;; Tree protocol

(defgeneric map-threads (tree)) ; TODO clashes with run protocol
