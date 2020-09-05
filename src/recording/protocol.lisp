;;;; protocol.lisp --- Protocol generic functions provided by the recording module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

;;; Recording lifecycle protocol
;;;
;;; These generic functions are called to prepare, start, stop and
;;; teardown recording of a given run using a given recorder.

(defgeneric setup (recorder run)
  (:documentation
   "Prepare RECORDER for recording into RUN.

Any preparation, especially if it is expensive, that can be done
without starting the recording should be done in a method on this
generic function."))

(defgeneric start (recorder run))

(defgeneric stop (recorder run))

(defgeneric teardown (recorder run))

;;; Source protocol
;;;
;;; Extends recording lifecycle protocol

(defgeneric add-chunk (source run chunk)
  (:documentation
   "Add CHUNK produced by SOURCE to run.

CHUNK is a sequence of events."))

(defgeneric note-source-thread (source run thread event)
  (:documentation
   "Inform the recorder that THREAD is used by SOURCE for RUN.

    EVENT is either :STARTED or :STOPPED and indicates whether THREAD
    was started or stopped.

    This function can only be called in the setup and teardown
    phases of the recording lifecycle."))

;;; Recorder protocol
;;;
;;; Extends recording lifecycle protocol
;;;
;;; Chunks submitted by sources are processed using `handle-chunk',
;;; individual items in chunks are processed using `handle-item'.
;;; Result are appended to the run.

(defgeneric make-run (recorder)) ; TODO make-builder? return two values: a run and a builder?

(defmethod handle-chunk (recorder run chunk)) ; TODO run -> builder

(defmethod handle-item (recorder run item))

;;; Recorder construction protocol

(defgeneric make-recorder (kind &rest args &key &allow-other-keys))
