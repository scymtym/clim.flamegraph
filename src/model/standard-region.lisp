;;;; standard-region.lisp ---
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `root-region-mixin'

(defclass root-region-mixin ()
  ((%thread :initarg :thread
            :reader  thread)))


;;; `standard-region'

(defclass standard-region (name-mixin
                           temporal-interval-mixin
                           print-items:print-items-mixin)
  ())

#+maybe (defmethod print-items:print-items append ((object region))
  (let+ (((&accessors-r/o name duration) object))
    `((:name        ,name)
      (:duration    ,duration " ~/text.orders-of-magnitude:print-human-readable-duration/" ((:after :name))))))

(defmethod start-time :around ((object standard-region)) ; TODO temp
  (/ (call-next-method) 1000000))

(defmethod end-time :around ((object standard-region))
  (when-let ((time (call-next-method)))
    (/ time 1000000)))

(defmethod print-object :around ((object standard-region) stream)
  (let ((*package* (find-package '#:clim.flamegraph.model))) ; HACK
    (call-next-method)))

;;; `inner-region'

(defclass inner-region (standard-region) ; TODO node has children as well
  ((%children :initarg  :children
              :accessor children        ; TODO adjustable-vector?
              :initform '())))

(defmethod print-items:print-items append ((object inner-region))
  `((:child-count ,(length (children object)) " (~D)" ((:after :duration)))))

;;; `call-region'

(defclass call-region (inner-region)
  ((%values :initarg :values
            :reader  values*)))

;;; `root-call-region'

(defclass root-call-region (root-region-mixin
                            call-region)
  ())

;;; `wait-region'

(defclass wait-region (call-region)     ; TODO should be region
  ((%lock :initarg :lock
          :reader  lock)
   (%values :initform '())))
