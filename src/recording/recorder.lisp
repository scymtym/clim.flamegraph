;;;; recorder.lisp --- .
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

(defclass standard-recorder ()
  ((%sources :initarg  :sources
             :type     list
             :reader   sources)
   (%thread  :accessor %thread)))

(defmethod make-recorder ((kind (eql :standard)) &rest args &key)
  (apply #'make-instance 'standard-recorder args))

(defmethod make-run ((recorder standard-recorder)) ; TODO rename
  (make-instance 'standard-run-builder))

;;; Lifecycle

(defmethod setup ((recorder standard-recorder) (run t))
  (map nil (rcurry #'setup run) (sources recorder))
  (values))

(defmethod setup ((recorder standard-recorder) (run standard-run-builder))
  (call-next-method)
  (setf (%thread recorder) (bt:make-thread (curry #'work recorder run)
                                           :name "recording worker"))
  (values))

(defmethod start ((recorder standard-recorder) (run t))
  (map nil (rcurry #'start run) (sources recorder)))

(defmethod stop ((recorder standard-recorder) (run t))
  (map nil (rcurry #'stop run) (sources recorder)))

(defmethod teardown ((recorder standard-recorder) (run t))
  (map nil (rcurry #'teardown run) (sources recorder))
  (values))

(defmethod teardown ((recorder standard-recorder) (run standard-run-builder))
  (call-next-method)
  (lparallel.queue:push-queue :end (%queue run))
  (bt:join-thread (%thread recorder))
  ;; TODO cut data-vector to size
  (values))

;;; Source

(defmethod note-source-thread ((source t)
                               (run    t)
                               (thread t)
                               (event  t))
  )

;;; Recording
;;;
;;; In a dedicated thread, the `work' method pops chunks off the
;;; recorder's queue and calls `handle-item' on each item in the
;;; chunk. The result is appended to the run.

(defmethod work ((recorder standard-recorder) (run standard-run-builder))
  (loop :with queue = (%queue run)
        :for item = (lparallel.queue:pop-queue queue)
        :until (eq item :end)
        :do (handle-chunk recorder run item)))

(defmethod handle-chunk ((recorder standard-recorder)
                         (run      t)
                         (chunk    t))
  (map nil (curry #'handle-item recorder run) chunk))
