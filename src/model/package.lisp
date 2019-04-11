(cl:defpackage #:clim.flamegraph.model
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:time)

  ;; Name protocol
  (:export
   #:name)

  ;; Temporal interval protocol
  (:export
   #:finished?
   #:start-time
   #:end-time)

  ;; Thread protocol (extends name protocol)
  (:export
   )

  ;; Run protocol
  (:export
   #:map-threads
   #:map-traces)

  ;; Trace protocol
  (:export
   #:thread
   #:time
   #:map-samples)

  ;; Sample protocol (extends name protocol)
  (:export
   )

  ;; Node protocol (extends name protocol)
  (:export
   #:hit-count
   #:children)

  ;; Tree protocol
  (:export
   #:map-threads)

  (:export
   #:name-mixin))
