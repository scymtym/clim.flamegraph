;;;; package.lisp --- Package definition for the view.timeline module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.view.timeline
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model #:clim.flamegraph.model)))
