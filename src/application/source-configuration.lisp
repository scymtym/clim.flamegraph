(cl:in-package #:clim.flamegraph.application)

;;; `source-configuration'

(defclass source-configuration ()
  ((%sources :accessor sources
             :initform '())))

;;; Threads

(defvar *threads* '())
