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
   (%sample-rate    :initarg  :sample-rate
                    :type     (real (0) 1)
                    :reader   sample-rate
                    :initform 1)
   (%depth-limit    :initarg  :depth-limit ; TODO rename to max-depth
                    :type     (or null positive-integer)
                    :reader   depth-limit
                    :initform nil)
   (%min-duration   :initarg  :min-duration
                    :type     non-negative-real ; TODO unit and type?
                    :reader   min-duration
                    :initform 0)
   ;; Runtime state
   (%run            :accessor run)
   (%thread         :accessor thread))
  (:default-initargs
   :specification (error "missing required initarg :SPECIFICATION")))

(defmethod recording:setup ((source source) (run t))
  (format *trace-output* "~A setting up~%" source)

  ;; Advice functions for tracing according to the specification.
  (map nil (lambda (entry)
             (destructuring-bind (name &optional arguments?)
                 (ensure-list entry)
               (record name :arguments? arguments?)))
       (specification source))

  ;; Set up a recording state with the specified parameters.
  (let ((depth-limit (or (depth-limit source) +unlimited+)))
    (setf *recording-state* (make-recording-state depth-limit)))

  ;; Start a worker thread and tell the recorder about it.
  (setf (run source) run)
  (let ((thread (bt:make-thread (curry #'work run source)
                                :name "advice source worker")))
    (setf (thread source) thread)
    (recording:note-source-thread source run thread :started)))

(defmethod recording:start ((source source) (run t)) ; TODO store state in the source?
  (format *trace-output* "~A start~%" source)

  (setf (recording-state-recording? *recording-state*) t))

(defmethod recording:stop ((source source) (run t))
  (format *trace-output* "~A stop~%" source)

  (setf (recording-state-recording? *recording-state*) nil))

(defmethod recording:teardown ((source source) (run t))
  (format *trace-output* "~A tearing down~%" source)

  (setf (recording-state-recording? *recording-state*) :terminating)
  (let ((thread (thread source)))
    (bt:join-thread thread)
    (recording:note-source-thread source run thread :stopped))

  (setf *recording-state* nil)

  ;; Advice functions for tracing according to the specification.
  (map nil (compose #'unrecord #'ensure-car) (specification source)))
