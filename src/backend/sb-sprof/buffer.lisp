;;;; buffer.lisp --- Ringbuffer for traces used by the backend.sb-sprof module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.sb-sprof)

(defstruct (trace-buffer
            (:constructor make-trace-buffer (&key (depth-limit 1024))))
  (thread  nil)
  (time    0                              :type (unsigned-byte 62)) ; TODO type
  (samples (make-array (* 2 depth-limit)) :type simple-vector :read-only t)
  (count   0                              :type array-index))

(defun make-trace-ring-buffer (&key (depth-limit 1024))
  (coerce (loop :repeat 128 :collect (make-trace-buffer :depth-limit depth-limit))
          '(simple-array trace-buffer (128))))

(defstruct (context
            (:constructor make-context (&key (depth-limit 1024))))
  (depth-limit 0                                                 :type array-index :read-only t)
  (traces      (make-trace-ring-buffer :depth-limit depth-limit) :type (simple-array trace-buffer (128)) :read-only t)
  (read-head   0                                                 :type array-index)
  (write-head  0                                                 :type array-index))

(defun produce-trace (context)
  (let ((write-head (context-write-head context)))
    (aref (context-traces context) (mod write-head 128)) ; TODO magic number
    ))

(defun maybe-produce-trace (context)
  (let ((read-head  (context-read-head context))
        (write-head (context-write-head context)))
    (when (< (- write-head read-head) 4 #+no 128)
      (produce-trace context))))

(defun commit-trace (context)
  (incf (context-write-head context)))

(defun consume-trace (context)
  (let ((read-head (context-read-head context)))
    (prog1
        (aref (context-traces context) (mod read-head 128))
      (setf (context-read-head context) (1+ read-head)))))

(defun maybe-consume-trace (context)
  (let ((read-head  (context-read-head context))
        (write-head (context-write-head context)))
    (when (< read-head write-head)
      (consume-trace context))))
