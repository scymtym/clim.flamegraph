;;;; backend.lisp ---
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.advice)

;;; Sample patterns
;;;
;;; A sample patterns statically indicates which subset of a sequence
;;; of sampler executions should actually sample such that a given
;;; frequency is asymptotically achieved without any `random' calls
;;; during sampler execution.

(deftype sample-pattern-array ()
  '(simple-array bit (1024)))

(defun make-sample-pattern-array (rate period-length)
  (map-into (make-array period-length :element-type 'bit)
            (lambda ()
              (if (<= (random 1.0) rate)
                  1
                  0))))

(let* ((pattern (make-sample-pattern-array .01 1024))
       (count   (count 1 pattern))
       (length  (length pattern)))
  (values pattern count length (float (/ count length) 1.0)))

(defstruct (sample-pattern
            (:constructor make-sample-pattern
                (rate period-length
                 &aux (array (make-sample-pattern-array rate period-length)))))
  (array nil :type sample-pattern-array :read-only t)
  (index 0   :type (integer 0 1024)))

(declaim (inline sample?))
(defun sample? (pattern)
  (or (not pattern)
      (let ((index (sample-pattern-index pattern)))
        (prog1
            (= 1 (aref (sample-pattern-array pattern) index))
          (setf (sample-pattern-index pattern) (mod (1+ index) 1024))))))

;;; Limits

(define-constant +unlimited+
    (1- (ash 1 62)))

;;;;

(defstruct (event
            (:constructor make-event)
            (:copier      nil))
  (kind)
  (name)
  (time   0 :type (unsigned-byte 62))   ; TODO type
  (values))

(defconstant +event-ring-buffer-size+ 65536)

(deftype ring-buffer-array () ; TODO event-ring-buffer-array
  `(simple-array t (,+event-ring-buffer-size+)))

(defun make-event-ring-buffer ()
  (coerce (loop :repeat +event-ring-buffer-size+ :collect (make-event))
          `(simple-array event (,+event-ring-buffer-size+))))

(defstruct (thread-state ; TODO rename to something-buffer
            (:constructor make-thread-state
                (depth-limit duration-limit sample-rate count-limit
                 &aux (sample-pattern (when (and sample-rate (/= sample-rate 1))
                                        (make-sample-pattern sample-rate 1024)))))
            (:copier nil)
            (:predicate nil))
  ;; Parameters
  (depth-limit    0                        :type (unsigned-byte 62))
  (duration-limit 0                        :type (unsigned-byte 62)) ; TODO time type
  (sample-pattern nil                      :type (or null sample-pattern))
  (count-limit    1000000                  :type (unsigned-byte 62))
  ;; Runtime state
  (recording?     t                        :type boolean)
  (depth          0                        :type (unsigned-byte 62))
  ;; Ring buffer
  (events         (make-event-ring-buffer) :type ring-buffer-array :read-only t)
  (read-head      0                        :type array-index)
  (write-head     0                        :type array-index))

(defun produce-event (thread-state)
  (let ((write-head (thread-state-write-head thread-state)))
    (aref (thread-state-events thread-state)
          (mod write-head +event-ring-buffer-size+))))

(defun maybe-produce-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    ; TODO count overruns
    (when (< (- write-head read-head) +event-ring-buffer-size+)
      (produce-event thread-state))))

(defun commit-event (thread-state)
  (incf (thread-state-write-head thread-state)))

(defun consume-event (thread-state)
  (let ((read-head (thread-state-read-head thread-state)))
    (prog1
        (aref (thread-state-events thread-state)
              (mod read-head +event-ring-buffer-size+))
      (setf (thread-state-read-head thread-state) (1+ read-head)))))

(defun maybe-consume-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    (when (< read-head write-head)
      (consume-event thread-state))))

(defstruct recording-state
  ;; Parameters
  (depth-limit    0                                            :type (unsigned-byte 62))
  (duration-limit 0                                            :type (unsigned-byte 62)) ; TODO time type
  (sample-rate    1                                            :type (real (0) 1))
  (count-limit    1000000                                      :type (unsigned-byte 62))
  (thread-test    nil                                          :type (or null function))
  ;; Runtime state
  (recording?     nil                                          :type (or (eql :terminating) boolean))
  (thread-states  (make-hash-table :test #'eq :synchronized t) :type hash-table :read-only t))

(declaim (inline ensure-thread-state))
(defun ensure-thread-state (recording-state
                            &optional (thread (bt:current-thread)))
  (let ((table (recording-state-thread-states recording-state)))
    (ensure-gethash
     thread table
     (let ((test (recording-state-thread-test recording-state)))
       (if (or (null test) (funcall test thread))
           (make-thread-state
            (recording-state-depth-limit recording-state)
            (recording-state-duration-limit recording-state)
            (recording-state-sample-rate recording-state)
            (recording-state-count-limit recording-state))
           :ignore)))))

;;; Macros

(defvar *recording-state* nil)

(defvar *thread-state* nil)

(defmacro with-recording-state ((state-var) recording-body normal-body)
  `(let ((,state-var *recording-state*))
     (if (and ,state-var
              (eq (recording-state-recording? ,state-var) t)
              (not recording:*in-critical-recording-code?*))
         (#+sbcl sb-sys:without-interrupts #-sbcl progn
          (#+sbcl sb-sys:allow-with-interrupts #-sbcl progn
           ,recording-body))
         ,normal-body)))

(defmacro with-thread-state ((state-var &optional thread)
                             recording-body normal-body)
  (with-gensyms
      (recording-state recording-thunk normal-thunk with-state-thunk count-var)
    `(labels ((,recording-thunk (,state-var)
                ,recording-body)
              (,normal-thunk ()
                (let ((*in* nil))
                  ,normal-body))
              (,with-state-thunk (,state-var)
                (if (and (thread-state-recording? ,state-var)
                         (< (thread-state-depth ,state-var)
                            (thread-state-depth-limit ,state-var))
                         (sample? (thread-state-sample-pattern ,state-var)))
                    (let ((,count-var (thread-state-count-limit ,state-var))) ; TODO the body may produce two events
                      (cond ((plusp ,count-var)
                             #+no (when (zerop (mod ,count-var 100))
                               (print (list ,count-var (bt:current-thread)) *trace-output*))
                             (setf (thread-state-count-limit ,state-var) (1- ,count-var))
                             (,recording-thunk ,state-var))
                            (t
                             (,normal-thunk))))
                    (,normal-thunk))))
       ; (declare (inline ,recording-thunk ,normal-thunk ,with-state-thunk))
       (with-recording-state (,recording-state)
         (let ((,state-var *thread-state*))
           (case ,state-var
             ((nil)
              (let* ((,state-var     (ensure-thread-state
                                      ,recording-state ,@(when thread `(,thread))))
                     (*thread-state* ,state-var))
                (if (eq ,state-var :ignore)
                    (,normal-thunk)
                    (,with-state-thunk ,state-var))
                #+no (,with-state-thunk ,state-var)))
             (:ignore
              (,normal-thunk))
             (t
              (,with-state-thunk ,state-var))))
         (,normal-thunk)))))

(defvar *in* nil)
(defvar *context* nil)
(defvar *dropped-count* (cons 0 nil))

(defmacro with-thread-state-and-nesting ((state-var &optional thread)
                                         recording-body normal-body)
  (with-gensyms (old-depth)
    `(if *in*
         (progn
           (sb-ext:atomic-incf (car *dropped-count*))
           (let ((*in* nil))
             ,normal-body))
         (let ((*in* t))
           (with-thread-state (,state-var ,thread)
             (let ((,old-depth (thread-state-depth ,state-var)))
               (setf (thread-state-depth ,state-var) (1+ ,old-depth))
               (multiple-value-prog1
                   ,recording-body
                 (setf (thread-state-depth ,state-var) ,old-depth))) ; TODO what about non-local exits?
             ,normal-body)))))
