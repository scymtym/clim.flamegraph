;;;; package.lisp --- Package definition for the backend.advice module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.backend.advice
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:time      #:clim.flamegraph.time)
   (#:model     #:clim.flamegraph.model)

   (#:recording #:clim.flamegraph.recording)
   (#:backend   #:clim.flamegraph.backend)))
