;;;; package.lisp --- Package definition for the clim.flamegraph system.
;;;;
;;;; Copyright (C) 2017-2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:trace)

  (:shadowing-import-from #:alexandria
   #:simple-parse-error)

  (:export
   #:flamegraph))
