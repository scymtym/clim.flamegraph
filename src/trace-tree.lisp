;;;; trace-tree.lisp --- Conditions used in the options system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Utilities

(defun map-filtered-traces (function traces &key filter)
  (let ((filter (when filter
                  (ensure-function filter))))
    (sb-sprof:map-traces
     (lambda (thread time trace)
       (when (or (not filter)
                 (funcall filter thread time trace))
         (funcall function thread time trace)))
     traces)))

(defun select-traces (traces &key start-time end-time)
  (let ((result '()))
    (map-filtered-traces
     (lambda (thread time trace)
       (declare (ignore thread time))
       (push trace result))
     traces :filter (lambda (thread time trace)
                      (declare (ignore thread trace))
                      (and (or (not start-time) (<= start-time time))
                           (or (not end-time)   (<= time end-time)))))
    result))

;;; Trace tree

(defstruct (node
            (:constructor make-node (&optional call (count 0))))
  (call     nil                                                        :read-only t)
  (count    nil                             :type non-negative-integer)
  (children (make-hash-table :test #'equal) :type hash-table))

(defun node-name (node)
  (sb-sprof::node-name (node-call node)))

#+no (defun traces->tree (traces &key filter)
       (let ((root (make-node)))
         (sb-sprof::with-lookup-tables ()
                                       (map-traces
                                        (lambda (thread time trace)
                                          (declare (ignore thread time))
                                          (let ((node root))
                                            (incf (node-count node))
                                            (sb-sprof::map-calls
                                             (lambda (debug-info pc-offset)
                                               (declare (ignore pc-offset))
                                               (when-let ((call (sb-sprof::lookup-node debug-info))) ; TODO should not be nil
                                                 (setf node (ensure-gethash
                                                             (sb-sprof::node-name call)
                                                             (node-children node)
                                                             (make-node call)))
                                                 (incf (node-count node))))
                                             trace)))
                                        traces :filter filter))
         root))

(defun traces->tree (traces)
  (let ((root (make-node)))
    (sb-sprof::with-lookup-tables ()
      (map nil (lambda (trace)
                 (let ((node root))
                   (incf (node-count node))
                   (sb-sprof:map-trace-samples
                    (lambda (info pc-or-offset)
                      (declare (ignore pc-or-offset))
                      (when info
                        (when-let ((call (sb-sprof::lookup-node info))) ; TODO can this be nil even if INFO isn't?
                          (setf node (ensure-gethash
                                      (sb-sprof::node-name call)
                                      (node-children node)
                                      (make-node call)))
                          (incf (node-count node)))))
                    trace)))
           traces))
    root))
