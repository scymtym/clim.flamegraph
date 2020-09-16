;;;; source.lisp --- The advice-based event source.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.advice)

(defclass source (backend:omit-trace-mixin)
  (;; Parameters
   (%specification  :initarg  :specification
                    :reader   specification)
   (%depth-limit    :initarg  :depth-limit ; TODO rename to max-depth
                    :type     (or null positive-integer)
                    :reader   depth-limit
                    :initform nil)
   (%min-duration   :initarg  :min-duration
                    :type     non-negative-real ; TODO unit and type?
                    :reader   min-duration
                    :initform 0)
   (%sample-rate    :initarg  :sample-rate
                    :type     (real (0) 1)
                    :reader   sample-rate
                    :initform 1)
   ;; TODO better name. this is max samples per thread
   (%count-limit    :initarg  :count-limit
                    :type     positive-integer
                    :reader   count-limit
                    :initform 1000000)
   ;; TODO thread filter?
   (%thread-test    :initarg  :thread-test
                    :type     (or null function)
                    :reader   thread-test
                    :initform nil)
   ;; Runtime state
   (%sink           :accessor sink)
   (%thread         :accessor thread))
  (:default-initargs
   :specification (error "missing required initarg :SPECIFICATION")))

(defmethod recording:setup ((source source) (sink t))
  (format *trace-output* "~A setting up~%" source)

  ;; Advice functions for tracing according to the specification.
  (map nil (lambda (entry)
             (destructuring-bind (name &optional arguments?)
                 (ensure-list entry)
               (record name :arguments? arguments?)))
       (specification source))

  ;; Set up a recording state with the specified parameters.
  (let ((depth-limit  (or (depth-limit source) +unlimited+))
        (min-duration (min-duration source))
        (sample-rate  (sample-rate source))
        (thread-test  (thread-test source)))
    (setf *recording-state* (make-recording-state :depth-limit    depth-limit
                                                  :duration-limit min-duration
                                                  :sample-rate    sample-rate
                                                  :count-limit    1000000
                                                  :thread-test    thread-test)))

  ;; Start a worker thread and tell the recorder about it.
  (setf (sink source) sink)
  (let ((thread (bt:make-thread (curry #'work source sink)
                                :name "advice source worker")))
    (setf (thread source) thread)
    (recording:note-source-thread source sink thread :started)))

(defmethod recording:start ((source source) (sink t)) ; TODO store state in the source?
  (format *trace-output* "~A start~%" source)

  (setf (recording-state-recording? *recording-state*) t))

(defmethod recording:stop ((source source) (sink t))
  (format *trace-output* "~A stop~%" source)

  (setf (recording-state-recording? *recording-state*) nil))

(defmethod recording:teardown ((source source) (sink t))
  (format *trace-output* "~A tearing down~%" source)

  (setf (recording-state-recording? *recording-state*) :terminating)
  (let ((thread (thread source)))
    (bt:join-thread thread)
    (recording:note-source-thread source sink thread :stopped))

  (setf *recording-state* nil)

  ;; Advice functions for tracing according to the specification.
  (map nil (compose #'unrecord #'ensure-car) (specification source)))
