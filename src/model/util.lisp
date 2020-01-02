;;;; util.lisp --- Utilities provided by the model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

(defmethod name-string ((name null) &key qualified?)
  (declare (ignore qualified?))
  "<root>")

(defmethod name-string ((name symbol) &key qualified?)
  (if qualified?
      (let ((*package* (find-package :keyword)))
        (prin1-to-string name))
      (symbol-name name)))

(defmethod name-string ((name t) &key qualified?)
  (declare (ignore qualified?))
  (let ((*print-pretty* nil))
    (princ-to-string name)))
