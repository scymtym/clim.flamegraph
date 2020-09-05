;;;; standard-function.lisp --- Representation of function and associated statistics.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `standard-function'
;;;
;;; A place to aggregate information for a given function such as call
;;; counts and total time.

(defclass standard-function (name-mixin
                             print-items:print-items-mixin)
  (;; Threads
   (%calling-threads         :initarg  :calling-threads
                             :type     list
                             :accessor calling-threads
                             :initform '())
   ;; Calls (from deterministic profiling)
   (%call-count              :initarg  :call-count
                             :type     non-negative-integer
                             :accessor call-count
                             :initform 0)
   (%total-run-time          :initarg  :total-run-time
                             :type     non-negative-real
                             :accessor total-run-time
                             :initform 0)
   ;; Hits (from statistical profiling)
   (%hit-count               :initarg  :hit-count
                             :type     non-negative-integer
                             :accessor hit-count
                             :initform 0)
   (%non-recursive-hit-count :initarg  :non-recursive-hit-count
                             :type     non-negative-integer
                             :accessor non-recursive-hit-count
                             :initform 0)
   (%self-hit-count          :initarg  :self-hit-count
                             :type     non-negative-integer
                             :accessor self-hit-count
                             :initform 0)))

(defmethod name-string ((name standard-function) &key qualified?) ; TODO hack
  (name-string (name name) :qualified? qualified?))

(defmethod print-items:print-items append ((object standard-function))
  `((:call-count     ,(call-count object)     " (~D)" ((:after :name)))
    ; (:total-run-time ,(total-run-time object) ""      ((:after :call-count)))
    ))

;;; `call-mixin'

(defclass call-mixin ()
  ((%called-function :initarg :called-function
                     :reader  called-function))
  (:default-initargs
   :called-function (error "Missing required :CALLED-FUNCTION initarg")))

(defmethod print-items:print-items append ((object call-mixin))
  (list (find :name (print-items:print-items (called-function object))
              :test #'eq :key #'first)))
