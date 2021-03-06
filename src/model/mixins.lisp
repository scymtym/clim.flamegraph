;;;; mixins.lisp --- Mixin classes provided by the model module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `name-mixin'

(defclass name-mixin ()
  ((%name :initarg :name
          :reader  name))
  (:default-initargs
   :name (error "Missing required :NAME initarg")))

(defmethod print-items:print-items append ((object name-mixin))
  (let ((name (name-string (name object) :qualified? t)))
    `((:name ,name "~A"))))

;;; `temporal-point-mixin'

(defclass temporal-point-mixin ()
  ((%time :initarg :time
          :reader  time))
  (:default-initargs
   :time (error "Missing required :TIME initarg")))

;;; `temporal-interval-mixin'

(defclass temporal-interval-mixin ()
  ((%start-time :initarg  :start-time
                :reader   %start-time
                :reader   start-time)
   (%end-time   :initarg  :end-time
                :reader   %end-time
                :accessor end-time
                :initform nil)))

(defmethod print-items:print-items append ((object temporal-interval-mixin))
  `((:duration ,(duration* object) "~/clim.flamegraph.time:print-human-readable-duration/" ((:after :name)))))
