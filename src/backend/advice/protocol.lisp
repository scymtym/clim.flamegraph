;;;; protocol.lisp --- Protocol functions provided by the backend.advice module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.region)

;;; Region protocol

(defgeneric name (region))

(defgeneric start (region))

(defgeneric end (region))

(defgeneric duration (region))

(defgeneric children (region)
  (:method ((region t))
    '()))
