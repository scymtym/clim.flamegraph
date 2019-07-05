;;;; source.lisp --- The advice-based event source.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
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
   (%depth-limit    :initarg  :depth-limit
                    :type     (or null positive-integer)
                    :reader   depth-limit
                    :initform nil)
   (%duration-limit :initarg  :duration-limit
                    :type     (or null positive-integer) ; TODO unit?
                    :reader   duration-limit
                    :initform nil)
   ;; Runtime state
   (%run            :accessor run)
   (%thread         :accessor thread))
  (:default-initargs
   :specification (error "missing required initarg :SPECIFICATION")))

(defmethod recording:setup ((source source) (run t))
  ;; Advice functions for tracing according to the specification.
  (map nil #'record (specification source))

  ;; Set up a recording state with the specified parameters.
  (setf *recording-state* (make-recording-state (or (depth-limit source)    (1- (ash 1 62)))
                                                (or (duration-limit source) (1- (ash 1 62)))))

  ;; Start a worker thread and tell the recorder about it.
  (setf (run source) run)
  (let ((thread (bt:make-thread (curry #'work run source)
                                :name "advice source worker")))
    (setf (thread source) thread)
    (recording:note-source-thread source run thread :started)))

(defmethod recording:start ((source source) (run t)) ; TODO store state in the source?
  (setf (recording-state-recording? *recording-state*) t))

(defmethod recording:stop ((source source) (run t))
  (setf (recording-state-recording? *recording-state*) nil))

(defmethod recording:teardown ((source source) (run t))
  (setf (recording-state-recording? *recording-state*) :terminating)

  (let ((thread (thread source)))
    (bt:join-thread thread)
    (recording:note-source-thread source run thread :stopped))

  (setf *recording-state* nil)

  ;; Advice functions for tracing according to the specification.
  (map nil #'unrecord (specification source)))
