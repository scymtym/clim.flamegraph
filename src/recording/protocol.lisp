(cl:in-package #:clim.flamegraph.recording)

;;; Recording lifecycle protocol

(defgeneric setup (recorder run))

(defgeneric start (recorder run))

(defgeneric stop (recorder run))

(defgeneric teardown (recorder run))

;;; Source protocol
;;;
;;; Extends recording lifecycle protocol

(defgeneric add-chunk (source run chunk))

;;; Recorder protocol
;;;
;;; Extends recording lifecycle protocol

(defgeneric make-run (recorder))

(defmethod handle-chunk (recorder run chunk))

(defmethod handle-item (recorder run item))

;;; Recorder construction protocol

(defgeneric make-recorder (kind &rest args &key))
