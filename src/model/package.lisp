;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.model
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:time)

  ;; Utilities
  (:export
   #:name-string)

  ;; Name protocol
  (:export
   #:name)

  ;; Temporal interval protocol
  (:export
   #:finished?
   #:start-time
   #:end-time
   #:duration)

  ;; Thread protocol (extends name protocol)
  (:export
   )

  ;; Run protocol
  (:export
   #:map-threads   #:threads
   #:map-functions #:functions
   #:map-traces    #:traces)

  ;; Trace protocol
  (:export
   #:thread
   #:time ; TODO temporal point/event protocol
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
