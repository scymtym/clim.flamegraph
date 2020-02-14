;;;; buffer.lisp --- Ring buffer operations used by the backend.mezzano-profiler module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.mezzano-profiler)

(defstruct (context (:constructor make-context (&key (depth-limit 1024))))
  (depth-limit 0 :type array-index :read-only t))

(defun consume-traces (context)
  (declare (ignore context))
  (let ((buffer    mezzano.supervisor::*profile-buffer*)
        (read-head mezzano.supervisor::*profile-buffer-tail*))
    (when (eq (aref buffer read-head) :start)
      (let ((time (aref buffer (+ read-head 1))))
        (loop :repeat 100
              :for index = (+ read-head 2) :then new-index
              :for (trace new-index) = (multiple-value-list
                                        (consume-one-trace buffer index time))
              :while trace
              :collect trace
              :finally (setf mezzano.supervisor::*profile-buffer-tail* index))))))

(defun consume-one-trace (buffer start-index time)
  (when (not (bt:threadp (aref buffer start-index)))
    (return-from consume-one-trace nil))
  (let ((thread (aref buffer (+ start-index 0)))
        (state  (aref buffer (+ start-index 1)))
        (wait   (aref buffer (+ start-index 2)))
        (frames (make-array 0 :adjustable t :fill-pointer 0))) ; TODO
    (loop :for index :from (+ start-index 3) :below (1- (length buffer)) :by 2
          :for entry = (aref buffer index)
          :while (functionp entry)
          :do (vector-push-extend entry frames)
              (vector-push-extend (aref buffer (+ index 1)) frames)
          :finally (return (values (buffer->trace time thread state wait frames)
                                   index)))))

(defun maybe-consume-traces (context)
  (let ((read-head  mezzano.supervisor::*profile-buffer-tail*)
        (write-head mezzano.supervisor::*profile-buffer-head*))
    (when (< read-head write-head)
      (consume-traces context))))
