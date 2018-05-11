;;;; conditions.lisp --- Conditions used in the options system.
;;;;
;;;; Copyright (C) 2011-2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

(defun flamegraph (&optional (samples sb-sprof::*samples*))
  (clim:run-frame-top-level (clim:make-application-frame 'flamegraph :traces samples)))
