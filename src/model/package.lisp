(cl:defpackage #:clim.flamegraph.model
  (:use
   #:cl)

  (:shadow
   #:time)

  ;; Name protocol
  (:export
   #:name)

  ;; Temporal range protocol
  (:export
   )

  ;; Thread protocol (extends name protocol)
  (:export
   )

  ;; Run protocol
  (:export
   #:start-time
   #:end-time
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
