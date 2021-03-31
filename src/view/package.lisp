;;;; package.lisp --- Package definition for the view module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.view
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:time  #:clim.flamegraph.time)
   (#:model #:clim.flamegraph.model))

  ;; Utilities
  (:export
   #:package-color
   #:symbol-color)

  ;; Presentation types
  (:export
   #:name

   #:hit-count
   #:call-statistics
   #:called-function))
