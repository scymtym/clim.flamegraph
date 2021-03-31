;;;; run-builder.lisp --- .
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

(defclass standard-run-builder ()
  ((%data :accessor data
          :initform '())
   #+maybe-later (%run   :accessor run
                         :initform (make-instance 'model::standard-run))))

(defmethod handle-chunk ((recorder t) (sink standard-run-builder) (chunk t))
  (map nil (curry #'handle-item recorder sink) chunk))

(defmethod handle-item ((recorder t) (sink standard-run-builder) (item t))
                                        ; TODO vector-push-extend
  (push item (data sink)))
