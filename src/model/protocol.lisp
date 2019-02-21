(cl:in-package #:clim.flamegraph.model)

;;; Name protocol

(defgeneric name (object))

;;; Run protocol
;;;
;;; The run, trace and node protocols form the raw view onto collected
;;; data: a run collects traces at certain points in time. Each trace
;;; consists of a list of samples corresponding to the stack of active
;;; function calls in a particular thread.

(defgeneric start-time (run))

(defgeneric end-time (run))

(defgeneric map-threads (function run))

(defgeneric map-traces (function run))

;;; Trace protocol

(defgeneric thread (trace))

(defgeneric time (trace))

(defgeneric map-samples (function trace))

;;; Node protocol
;;;
;;; Extends name protocol.
;;;
;;; The node protocol facilities viewing the collected data as a tree
;;; in which parents are callers and children are callees.

(defgeneric hit-count (node))

(defgeneric children (node))

;;; Tree protocol

(defgeneric map-threads (tree)) ; TODO clashes with run protocol
