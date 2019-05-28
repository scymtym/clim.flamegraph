;;;; backend.lisp ---
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.advice)

(defstruct (event
            (:constructor make-event)
            (:copier      nil))
  (kind)
  (name)
  (time   0 :type (unsigned-byte 62)) ; TODO type
  (values))

(defconstant +event-ring-buffer-size+ 65536)

(defun make-event-ring-buffer ()
  (coerce (loop :repeat +event-ring-buffer-size+ :collect (make-event))
          `(simple-array event (,+event-ring-buffer-size+))))

(defstruct (thread-state ; TODO rename to something-buffer
            (:constructor make-thread-state ())
            (:copier nil)
            (:predicate nil))
  (recording? t                        :type boolean)
  (events     (make-event-ring-buffer) :type (simple-array t (#.+event-ring-buffer-size+)) :read-only t)
  (read-head  0                        :type array-index)
  (write-head 0                        :type array-index))

(defun produce-event (thread-state)
  (let ((write-head (thread-state-write-head thread-state)))
    (aref (thread-state-events thread-state) (mod write-head +event-ring-buffer-size+)) ; TODO magic number
    ))

(defun maybe-produce-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    (when (< (- write-head read-head) +event-ring-buffer-size+)
      (produce-event thread-state))))

(defun commit-event (thread-state)
  (incf (thread-state-write-head thread-state)))

(defun consume-event (thread-state)
  (let ((read-head (thread-state-read-head thread-state)))
    (prog1
        (aref (thread-state-events thread-state) (mod read-head +event-ring-buffer-size+))
      (setf (thread-state-read-head thread-state) (1+ read-head)))))

(defun maybe-consume-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    (when (< read-head write-head)
      (consume-event thread-state))))

(defstruct (recording-state
            (:constructor make-recording-state ()))
  (recording?    nil                                          :type (or (eql :terminating) boolean))
  (thread-states (make-hash-table :test #'eq :synchronized t) :type hash-table :read-only t))

;;;

(defvar *recording-state* nil)

(defvar *thread-state* nil)

(declaim (inline ensure-thread-state))
(defun ensure-thread-state (recording-state
                            &optional (thread (bt:current-thread)))
  (let ((table (recording-state-thread-states recording-state)))
    (ensure-gethash thread table (make-thread-state))))

#+unused (defmacro with-recording-state ((state-var) &body body)
  `(#+sbcl sb-sys:without-interrupts #-sbcl progn
    (when-let ((,state-var *recording-state*))
      (when (recording-state-recording? ,state-var)
        (let ((*recording-state* nil))
          ,@body)))))

#+unsed (defmacro with-thread-state ((state-var &optional thread) &body body)
  (alexandria:with-gensyms (recording-state)
    `(with-recording-state (,recording-state)
       (let ((,state-var (ensure-thread-state
                          ,recording-state ,@(when thread `(,thread)))))
         (when (thread-state-recording? ,state-var)
           ,@body)))))

(defmacro with-recording-state2 ((state-var) recording-body normal-body)
  `(let ((,state-var *recording-state*))
     (if (and ,state-var
              (recording-state-recording? ,state-var)
              (not recording:*in-critical-recording-code?*))
         (#+sbcl sb-sys:without-interrupts #-sbcl progn
          (#+sbcl sb-sys:allow-with-interrupts #-sbcl progn
           ,recording-body))
         ,normal-body)))

(defmacro with-thread-state2 ((state-var &optional thread)
                              recording-body normal-body)
  (with-gensyms
      (recording-state recording-thunk normal-thunk with-state-thunk)
    `(labels ((,recording-thunk (,state-var)
                ,recording-body)
              (,normal-thunk ()
                ,normal-body)
              (,with-state-thunk (,state-var)
                (if (thread-state-recording? ,state-var)
                    (,recording-thunk ,state-var)
                    (,normal-thunk))))
       (with-recording-state2 (,recording-state)
         (if-let ((,state-var *thread-state*))
           (,with-state-thunk ,state-var)
           (let* ((,state-var     (ensure-thread-state
                                   ,recording-state ,@(when thread `(,thread))))
                  (*thread-state* ,state-var))
             (,with-state-thunk ,state-var)))
         (,normal-thunk)))))
