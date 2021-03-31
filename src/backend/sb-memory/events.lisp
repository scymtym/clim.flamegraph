;;;; events.lisp --- Events produce by the sb-memory backend.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.sb-memory)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *extra-spaces*
    #.(a:if-let ((symbol (find-symbol "+HEAP-SPACE-KEYWORDS+" "SB-VM")))
          `'(,@(remove :dynamic (symbol-value symbol)))
          '())))

(defun %make-event (name time used generation)
  (let ((properties (list* :used used
                           (when generation
                             (list :generation generation)))))
    (make-instance 'model::standard-event :name       name
                                          :time       time
                                          :properties properties)))

(defun make-event (name &key (time (time:real-time))
                             (used (regions-used-into
                                    (make-regions-used-array)))
                             generation)
  (%make-event name time used generation))

(defconstant +generation-count+
  (1+ sb-vm:+pseudo-static-generation+))

(defconstant +region-count+
  (+ +generation-count+ (length *extra-spaces*)))

(deftype regions-used-array ()
  `(simple-array (unsigned-byte 64) (,+region-count+)))

(declaim (inline make-regions-used-array))
(defun make-regions-used-array ()
  (make-array +region-count+ :element-type '(unsigned-byte 64)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct gc-info
    (start-time 0                         :type (unsigned-byte 64))
    (used       (make-regions-used-array) :type regions-used-array)))

(declaim (inline regions-used-into))
(defun regions-used-into (used)
  (loop :for i :below +generation-count+
        :do (setf (aref used i)
                  (sb-ext:generation-bytes-allocated i)))
  ;; Immobile spaces
  (macrolet
      ((extra-spaces ())
             (when-let ((spaces *extra-spaces*))
               `(setf ,@(loop :for i :from 0 :for space :in spaces
                              :collect `(aref used (+ +generation-count+ ,i))
                              :collect `(sb-vm:space-bytes ,space)))))
    (extra-spaces))
  used)

;;; This is pre-allocated so `note-gc-start' can write to it without
;;; consing.
(sb-ext:defglobal **gc-start** (make-gc-info))

(defun note-gc-start ()
  (let ((info **gc-start**))
    (setf (gc-info-start-time info) (time:real-time))
    (regions-used-into (gc-info-used info))))

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
                                  (regions-used-into
                                   (make-regions-used-array))
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
