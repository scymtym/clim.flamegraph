;;;; recorder.lisp --- .
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

(defclass standard-recorder ()
  (;; Configuration
   (%sources :initarg  :sources
             :type     list
             :reader   sources)
   ;; State
   (%queue   :reader   %queue
             :initform (lparallel.queue:make-queue))
   (%thread  :accessor %thread)))

(defmethod make-recorder ((kind (eql :standard)) &rest args &key)
  (apply #'make-instance 'standard-recorder args))

(defmethod make-run ((recorder standard-recorder)) ; TODO rename
  (make-instance 'standard-run-builder))

;;; Lifecycle

(defmethod setup ((recorder standard-recorder) (sink t))
  (map nil (rcurry #'setup recorder) (sources recorder))
  (values))

(defmethod setup ((recorder standard-recorder) (sink standard-run-builder))
  (call-next-method)
  (setf (%thread recorder) (bt:make-thread (curry #'work recorder sink)
                                           :name "recording worker"))
  (values))

(defmethod start ((recorder standard-recorder) (sink t))
  (map nil (rcurry #'start recorder) (sources recorder)))

(defmethod stop ((recorder standard-recorder) (sink t))
  (map nil (lambda (source)
             (with-simple-restart (continue "Skip stopping source ~A" source)
               (stop source recorder)))
       (sources recorder)))

(defmethod teardown ((recorder standard-recorder) (sink t))
  (map nil (lambda (source)
             (with-simple-restart (continue "Skip tear-down of source ~A" source)
               (teardown source recorder)))
       (sources recorder))
  (values))

(defmethod teardown ((recorder standard-recorder) (sink standard-run-builder))
  (call-next-method)
  (lparallel.queue:push-queue :end (%queue recorder))
  (bt:join-thread (%thread recorder))
  ;; TODO cut data-vector to size
  (values))

;;; Source

(defmethod note-source-thread ((source t)
                               (sink   t)
                               (thread t)
                               (event  t))
  )

;;; Recording
;;;
;;; In a dedicated thread, the `work' method pops chunks off the
;;; recorder's queue and calls `handle-item' on each item in the
;;; chunk. The result is appended to the run.

(defmethod work ((recorder standard-recorder) (sink t))
  (loop :with queue = (%queue recorder)
        :for item = (lparallel.queue:pop-queue queue)
        :until (eq item :end)
        :do (handle-chunk recorder sink item)))

(defmethod add-chunk ((source t) (sink standard-recorder) (chunk t))
  (lparallel.queue:push-queue chunk (%queue sink)))
