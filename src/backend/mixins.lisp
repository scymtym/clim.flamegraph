;;;; mixins.lisp --- Mixin classes for source classes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend)

(defclass omit-trace-mixin ()
  ((%omit-trace-threads? :initarg  :omit-trace-threads?
                         :type     boolean
                         :reader   omit-trace-threads?
                         :initform t)
   (%omit-trace-frames?  :initarg  :omit-trace-frames?
                         :type     boolean
                         :reader   omit-trace-frames?
                         :initform t)))
