;;;; timeline.lisp --- Timeline view and presentations.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; `thread' class

(defclass thread ()
  ((thread    :initarg  :thread
              :reader   thread)
   #+no (selected? :initarg  :selected?
                   :accessor selected?
                   :initform t)))

(defmethod name ((object thread))
  (sb-thread:thread-name (thread object)))

;;; `selection'

(defclass selection ()
  ((interval :initarg  :interval
             :reader   interval)
   (threads  :initarg  :threads
             :reader   threads
             :initform (make-hash-table :test #'eq))))

(defun make-selection (start end)
  (make-instance 'selection
                 :interval (make-interval start end)))

(defmethod selected? ((thread t) (selection selection))
  (ensure-gethash thread (threads selection) t))

(defmethod (setf selected?) ((new-value t) (thread t) (selection selection))
  (setf (gethash thread (threads selection)) new-value))

;;; interval

(defclass interval ()
  ((start :initarg  :start
          :type     (or null non-negative-real)
          :accessor start
          :initform nil)
   (end   :initarg  :end
          :type     (or null non-negative-real)
          :accessor end
          :initform nil)))

(defun make-interval (start end)
  (make-instance 'interval :start start :end end))
