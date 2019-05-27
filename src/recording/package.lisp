(cl:defpackage #:clim.flamegraph.recording
  (:use
   #:cl
   #:alexandria)

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
