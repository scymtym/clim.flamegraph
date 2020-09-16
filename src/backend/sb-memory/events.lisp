;;;; events.lisp --- Events produce by the sb-memory backend.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defun %make-event (name time used generation)
  (let ((properties (list* :used used
                           (when generation
                             (list :generation generation)))))
    (make-instance 'model::standard-event :name       name
                                          :time       time
                                          :properties properties)))

(defun make-event (name &key (time (time:real-time))
                             (used (sb-vm::dynamic-usage))
                             generation)
  (%make-event name time used generation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct gc-info
    (start-time 0 :type (unsigned-byte 64))
    (used       0 :type (and fixnum (integer 0)))))

(sb-ext:defglobal **gc-start** (make-gc-info))

(defun note-gc-start ()
  (let ((info **gc-start**))
    (setf (gc-info-start-time info) (time:real-time)
          (gc-info-used       info) (sb-vm::dynamic-usage))))

(defun note-gc-end (source sink generation)
  (let* ((start-info **gc-start**)
         (start-time (gc-info-start-time start-info))
         (end-time   (time:real-time))
         (start      (%make-event :gc-start
                                  start-time
                                  (gc-info-used start-info)
                                  generation))
         (end        (%make-event :gc-end
                                  end-time
                                  (sb-vm::dynamic-usage)
                                  generation))
         (region     (make-instance 'model::standard-region
                                    :name       :gc
                                    :start-time start-time
                                    :end-time   end-time)))
    (record:add-chunk source sink (list start end region))))

(defun instrumented-sub-gc (source sink function generation)
  (declare (type function function))
  (note-gc-start)
  (prog1
      (funcall function generation)
    (note-gc-end source sink generation)))
