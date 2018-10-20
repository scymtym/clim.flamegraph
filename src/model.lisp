;;;; timeline.lisp --- Timeline view and presentations.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

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

(defmethod selected? (thread (selection selection))
  (ensure-gethash thread (threads selection) t))

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
