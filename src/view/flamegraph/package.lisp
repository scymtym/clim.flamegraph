;;;; package.lisp --- Package definition for the view.flamegraph module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.view.flamegraph
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model #:clim.flamegraph.model)

   (#:view  #:clim.flamegraph.view)))
