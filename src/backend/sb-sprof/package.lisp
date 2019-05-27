;;;; package.lisp --- Package definition for the backend.sb-sprof module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.backend.sb-sprof
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:time
   #:trace)

  (:local-nicknames
   (#:time      #:clim.flamegraph.time)
   (#:model     #:clim.flamegraph.model)

   (#:recording #:clim.flamegraph.recording)))
