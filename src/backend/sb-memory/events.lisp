;;;; events.lisp ---
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defun %make-event (name time used)
  (make-instance 'model::standard-event :name       name
                                        :time       time
                                        :properties (list :used used)))

(defun make-event (name &key (time (time:real-time))
                              (used (sb-vm::dynamic-usage)))
  (make-event name time used))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct gc-info
    (start-time 0 :type (unsigned-byte 64))
    (used       0 :type (and fixnum (integer 0)))))

(sb-ext:defglobal **gc-start** (make-gc-info))

(defun note-gc-start ()
  (let ((info **gc-start**))
    (setf (gc-info-start-time info) (time:real-time)
          (gc-info-used       info) (sb-vm::dynamic-usage))))

(defun note-gc-end (source sink)
  (let ((start (let ((info **gc-start**))
                 (%make-event :gc-start
                              (gc-info-start-time info)
                              (gc-info-used info))))
        (end   (%make-event :gc-end
                            (time:real-time)
                            (sb-vm::dynamic-usage))))
    (record:add-chunk source sink (list start end))))

(defun instrumented-sub-gc (source sink function &rest args)
  (declare (type function function)
           (dynamic-extent args))
  (note-gc-start)
  (prog1
      (apply function args)
    (note-gc-end source sink)))
