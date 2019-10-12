;;;; package.lisp --- Package definition for the view module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.view
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:om    #:text.orders-of-magnitude)

   (#:model #:clim.flamegraph.model))

  (:export
   #:package-color
   #:symbol-color))
