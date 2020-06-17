;;;; package.lisp --- Package definition for the backend module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:defpackage #:clim.flamegraph.backend
  (:use
   #:cl)

  (:export
   #:omit-trace-threads?
   #:omit-trace-frames?

   #:omit-trace-mixin))
