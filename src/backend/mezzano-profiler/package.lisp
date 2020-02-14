;;;; package.lisp --- Package definition for the backend.mezzano-profiler module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.backend.mezzano-profiler
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:time
   #:trace)

  (:local-nicknames
   (#:time      #:clim.flamegraph.time)
   (#:model     #:clim.flamegraph.model)

   (#:recording #:clim.flamegraph.recording))

  (:documentation
   "A source implementation that consumes data collected by Mezzano's
    built-in statistical profiler."))
