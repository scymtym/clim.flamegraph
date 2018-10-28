;;;; interface.lisp --- User-facing convenience functions.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

(defun flamegraph (&optional (samples sb-sprof::*samples*))
  (let ((frame (clim:make-application-frame 'flamegraph :traces samples)))
    (clim:run-frame-top-level frame)))
