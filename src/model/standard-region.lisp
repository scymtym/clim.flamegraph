;;;; standard-region.lisp ---
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `inner-region-mixin'

(defclass inner-region/single-mixin ()
  ((%child :initarg  :child
           :accessor child)))

(defmethod children ((node inner-region/single-mixin))
  (list (child node)))

(defmethod print-items:print-items append ((object inner-region/single-mixin))
  `((:child-count ,1 " (~D)" ((:after :duration)))))

(defclass inner-region-mixin ()       ; TODO node has children as well
  ((%children :initarg  :children
              :accessor children        ; TODO adjustable-vector?
              :initform '())))

(defmethod print-items:print-items append ((object inner-region-mixin))
  `((:child-count ,(length (children object)) " (~D)" ((:after :duration)))))

(defmethod register-functions ((node inner-region-mixin) (functions hash-table)
                               &key (thread #+maybe (error "missing keyword argument ~S" :thread)))
  (labels ((rec (node)
             (let ((function (register-function node functions)))
               (when thread
                 (pushnew thread (calling-threads function) :test #'eq))
               (incf (call-count function))
               (incf (total-run-time function) (duration node)))
             (map nil #'rec (children node))))
    (rec node)))

;;; `root-region-mixin'

(defclass root-region-mixin ()
  ((%thread :initarg :thread
            :reader  thread)))

;; TODO the function should be take a function instead of a hash-table, maybe?
(defmethod register-functions :around ((node root-region-mixin) (functions hash-table)
                                       &key (thread (thread node)))
  (call-next-method node functions :thread thread))

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

;;; `call-region'

(defclass values-mixin ()
  ((%values :initarg :values
            :reader  values*)))

(defclass call-region (standard-region)
  ())

(defclass call-region/leaf (call-region)
  ())

(defclass call-region/leaf/values (call-region
                                   values-mixin)
  ())

(defclass call-region/inner (call-region
                             inner-region-mixin)
  ())

(defclass call-region/inner/values (call-region
                                    inner-region-mixin
                                    values-mixin)
  ())

;; TODO root without children
(defclass call-region/root (call-region
                            inner-region-mixin
                            root-region-mixin)
  ())

(defclass call-region/root/values (call-region
                                   inner-region-mixin
                                   root-region-mixin
                                   values-mixin)
  ())

;;; `wait-region'

(defclass wait-region (standard-region)
  ((%object :initarg :object
            :reader  object)))

(defclass wait-region/leaf (wait-region)
  ())

(defclass wait-region/inner (wait-region
                             inner-region-mixin)
  ())

;; TODO root without children
(defclass wait-region/root (wait-region
                            inner-region-mixin
                            root-region-mixin)
  ())
