(cl:defpackage #:clim.flamegraph.recording
  (:use
   #:cl
   #:alexandria)

  (:export
   #:*in-critical-recording-code?*)

  ;; Recording lifecycle protocol
  (:export
   #:setup
   #:start
   #:stop
   #:teardown)

  ;; Source protocol
  (:export
   #:add-chunk)

  ;; Recorder protocol
  (:export
   #:make-run

   #:handle-chunk
   #:handle-item)

  ;; Recorder construction protocol
  (:export
   #:make-recorder)

  ;; Macros
  (:export
   #:with-recording))

(cl:in-package #:clim.flamegraph.recording)

(defvar *in-critical-recording-code?* nil)
