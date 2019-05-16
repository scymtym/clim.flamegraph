(cl:in-package #:clim.flamegraph.region)

;;; Region protocol

(defgeneric name (region))

(defgeneric start (region))

(defgeneric end (region))

(defgeneric duration (region))

(defgeneric children (region)
  (:method ((region t))
    '()))
