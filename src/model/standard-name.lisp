;;;; standard-name.lisp --- .
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

(defclass qualified-name (name-mixin)
  ((%container :initarg :container
               :reader  container)))

(defmethod name-string ((name qualified-name) &key qualified?)
  (if qualified?
      (format nil "~A::~A" (container name) (name name))
      (name name)))
