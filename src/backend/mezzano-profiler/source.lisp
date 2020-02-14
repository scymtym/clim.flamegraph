;;;; source.lisp --- Source implementation provided by the backend.mezzano-profiler module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.mezzano-profiler)

(defvar *context*)

;;; `source'
;;;
;;; This source produce traces by asynchronously interrupting threads
;;; and sampling the respective call stacks. A background thread
;;; converts raw trace buffers to properly represented traces.

(defclass source ()
  (;; Parameters
   (%sample-interval   :initarg  :sample-interval
                       :reader   sample-interval
                       :initform 0.01
                       :documentation
                       "Time in seconds between samples.")
   (%trace-depth-limit :initarg  :trace-depth-limit
                       :reader   trace-depth-limit
                       :initform 1024
                       :documentation
                       "Maximum recording call stack depth. Stack
                        frames beyond the limit will not be present in
                        the recorded trace.")
   ;; Runtime state
   (%run               :accessor run)
   (%thread            :accessor thread)))

(defmethod recording:setup ((source source) (run t))
  (setf *context* (make-context :depth-limit (trace-depth-limit source)))
  (setf (run    source) run
        (thread source) (let* ((name   "profiler source worker")
                               (stream (make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                      :title (format nil "~A console" name))))
                          (bt:make-thread (lambda ()
                                            (let ((*terminal-io* stream))
                                              (work run source)))
                                          :name name))))

(defmethod recording:start ((source source) (run t))
  (mezzano.supervisor:start-profiling))

(defmethod recording:stop ((source source) (run t))
  (mezzano.supervisor:stop-profiling))

(defmethod recording:teardown ((source source) (run t))
  (setf *context* nil) ; TODO put a condition variable into the context
  (bt:join-thread (thread source)))

;;; Worker
;;;
;;; The `work' function is executed by a background thread to
;;; asynchronously pull traces out of the ring buffer the context and
;;; convert them to the proper trace representation.

(defun work (run source)
  ;; Unfortunately, the globals are unbounds until profiling actually
  ;; starts.
  (loop :until (every #'boundp '(mezzano.supervisor::*profile-buffer*
                                 mezzano.supervisor::*profile-buffer-head*
                                 mezzano.supervisor::*profile-buffer-tail*))
        :do (sleep .001))

  (loop :for context = *context*        ; TODO termination
        :while context
        :for traces = (consume-traces context)
        :when traces
        :do (recording:add-chunk source run traces)
        :do (sleep .01)))

(defun buffer->trace (time thread state wait frames)
  (make-instance 'model::standard-trace
                 :thread  thread
                 :time    (floor time 1000)
                 :samples (when (plusp (length frames))
                            (loop :for i :from 0 :below (length frames) :by 2
                                  :collect (make-instance 'model::standard-sample
                                                          :name (info->name (aref frames i)))))))

(defun info->name (info)
  (princ-to-string info))
