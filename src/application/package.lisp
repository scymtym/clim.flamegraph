;;;; package.lisp --- Package definition for the application module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.application
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model      #:clim.flamegraph.model)

   (#:recording  #:clim.flamegraph.recording)

   (#:view       #:clim.flamegraph.view)
   (#:timeline   #:clim.flamegraph.view.timeline)
   (#:flat       #:clim.flamegraph.view.flat)
   (#:flamegraph #:clim.flamegraph.view.flamegraph))

  (:export
   #:launch
   #:show-run
   #:add-run))
