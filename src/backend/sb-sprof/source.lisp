;;;; source.lisp --- Source implementation provided by the backend.sb-sprof module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.sb-sprof)

(defvar *context*)

;;; `source'
;;;
;;; This source produces traces by asynchronously interrupting threads
;;; and sampling the respective call stacks. A background thread
;;; converts raw trace buffers to properly represented traces.

(defclass source ()
  (;; Parameters
   (%sample-interval   :initarg  :sample-interval
                       :reader   sample-interval
                       :initform 0.01
                       :documentation
                       "Time in seconds between samples.")
   (%trace-depth-limit :initarg  :trace-depth-limit
                       :reader   trace-depth-limit
                       :initform 1024
                       :documentation
                       "Maximum recording call stack depth. Stack
                        frames beyond the limit will not be present in
                        the recorded trace.")
   (%thread-test       :initarg  :thread-test
                       :type     (or null symbol function) ; TODO function-designator
                       :reader   thread-test
                       :initform nil)
   (%name-test         :initarg  :name-test
                       :type     (or null symbol function) ; TODO function-designator
                       :reader   name-test
                       :initform nil)
   ;; Runtime state
   (%sink              :accessor sink
                       :documentation
                       "The sink into which finalized traces should be
                        stored after conversion in the worker thread.")
   (%thread            :accessor thread
                       :documentation
                       "The worker thread which performs the
                        conversion from raw traces to finalized traces
                        and their submission to the run in the
                        background.")))

(defmethod recording:setup ((source source) (sink t))
  ;; Prepare a context and a worker which consumes raw traces from the
  ;; trace ringbuffer of the context. The context is global.
  (let ((thread-test (when-let ((test (thread-test source)))
                       (ensure-function test))))
    (setf *context* (make-context :depth-limit (trace-depth-limit source)
                                  :thread-test thread-test)))
  (setf (sink   source) sink
        (thread source) (bt:make-thread (curry #'work source sink)
                                        :name "sprof source worker"))
  ;; Install a signal handler that will be invoked periodically via a
  ;; timer and the "profile" signal and will produce and push raw
  ;; traces into the ringbuffer of the context.
  (sb-sys:enable-interrupt sb-unix:sigvtalrm #'sigprof-handler/cpu))

(defmethod recording:start ((source source) (sink t))
  ;; Configure a system timer to trigger our handler according to the
  ;; requested sample interval.
  (multiple-value-bind (seconds fractional-seconds)
      (truncate (sample-interval source))
    (let ((microseconds (truncate (* fractional-seconds 1000000))))
      (sb-unix:unix-setitimer :virtual
                              seconds microseconds
                              seconds microseconds))))

(defmethod recording:stop ((source source) (sink t))
  (sb-unix:unix-setitimer :profile 0 0 0 0))

(defmethod recording:teardown ((source source) (sink t))
  ;; (sb-sys:enable-interrupt sb-unix:sigvtalrm :default) ; TODO

  (setf *context* nil) ; TODO put a condition variable into the context
  (bt:join-thread (thread source)))

;;; Worker
;;;
;;; The `work' function is executed by a background thread to
;;; asynchronously pop trace buffers off the context and convert them
;;; to the proper trace representation.

(defun work (source sink)
  (loop :with name-test = (when-let ((test (name-test source)))
                            (ensure-function test))
        :for context = *context*        ; TODO termination
        :while context
        :do (loop :for trace-buffer = (maybe-consume-trace context)
                  :while trace-buffer
                  :collect (buffer->trace trace-buffer name-test) :into traces
                  :finally (recording:add-chunk source sink traces))
            (sleep .01)))

(defun buffer->trace (buffer name-test)
  (declare (type (or null function) name-test))
  (make-instance 'model::standard-trace
                 :thread  (trace-buffer-thread buffer)
                 :time    (trace-buffer-time buffer)
                 :samples (when (plusp (trace-buffer-count buffer))
                            (loop :with samples = (trace-buffer-samples buffer)
                                  :for i :downfrom (* 2 (1- (trace-buffer-count buffer))) :to 0 :by 2
                                  :for name = (info->name (aref samples i))
                                  :when (or (null name-test)
                                            (funcall name-test name))
                                    :collect (make-instance 'model::standard-sample :name name)))))

(defun info->name (info)
  (flet ((clean-name (name)
           (if (and (consp name)
                    (member (first name)
                            '(sb-c::xep sb-c::tl-xep sb-c::&more-processor
                              sb-c::top-level-form
                              sb-c::&optional-processor)))
               (second name)
               name)))
    (typecase info
      (sb-kernel::code-component
       (let ((start (sb-sprof::code-bounds info)))
         (values (or (sb-disassem::find-assembler-routine start)
                     (format nil "~a" info))
                 info)))
      (sb-di::compiled-debug-fun
       (let* ((name      (sb-di::debug-fun-name info))
              (component (sb-di::compiled-debug-fun-component info))
              (start-pc  (sb-sprof::code-start component)))
         ;; Call graphs are mostly useless unless we somehow
         ;; distinguish a gazillion different (LAMBDA ())'s.
         (when (equal name '(lambda ()))
           (setf name (format nil "Unknown component: #x~x" start-pc)))
         (values (clean-name name) component)))
      (sb-di::debug-fun
       (clean-name (sb-di::debug-fun-name info)))
      (symbol
       (symbol-name info))
      (t
       (coerce info 'string)))))

;;; Signal handler
;;;
;;; This code was initially based on the signal handler code in SBCL's
;;; sb-sprof contrib but has somewhat diverged by now.

(defun collect-stacktrace (scp depth-limit buffer)
  (declare (optimize speed))
  (sb-sys:with-code-pages-pinned (:dynamic)
    (sb-alien:with-alien ((scp (* sb-sys:os-context-t) :local scp))
      (let* ((pc-ptr (sb-vm:context-pc scp))
             (fp     (sb-vm::context-register
                      scp #+x86-64 #.sb-vm::rbp-offset
                          #+x86 #.sb-vm::ebp-offset)))
        ;; Foreign code might not have a useful frame pointer in
        ;; ebp/rbp, so make sure it looks reasonable before walking
        ;; the stack
        (unless (sb-di::control-stack-pointer-valid-p (sb-sys:int-sap fp))
          (return-from collect-stacktrace nil))
        (let ((fp (sb-sys:int-sap fp))   ; TODO put into loop?
              (ok t))
          (declare (type sb-alien:system-area-pointer fp pc-ptr)
                   ;; FIXME: How annoying. The XC doesn't store enough
                   ;; type information about SB-DI::X86-CALL-CONTEXT,
                   ;; even if we declaim the ftype explicitly in
                   ;; src/code/debug-int. And for some reason that type
                   ;; information is needed for the inlined version to
                   ;; be compiled without boxing the returned saps. So
                   ;; we declare the correct ftype here manually, even
                   ;; if the compiler should be able to deduce this
                   ;; exact same information.
                   #+no (ftype (function (sb-alien:system-area-pointer)
                                         (values (member nil t)
                                                 sb-alien:system-area-pointer
                                                 sb-alien:system-area-pointer))
                               sb-di::x86-call-context))
          (loop :with samples = (trace-buffer-samples buffer)
                :for frame :of-type array-index :below depth-limit
                :for i :of-type fixnum :from 0 :by 2
                :for (info pc-or-offset) = (multiple-value-list
                                            (sb-sprof::debug-info pc-ptr))
                :do (setf (aref samples (+ i 0)) info
                          (aref samples (+ i 1)) pc-or-offset)
                :do (setf (values ok pc-ptr fp)
                          (sb-di::x86-call-context fp))
                :when (not ok)
                :do (setf (trace-buffer-count buffer) frame)
                    ;; If we fail to walk the stack beyond the initial
                    ;; frame, there is likely something wrong. Undo
                    ;; the trace start marker and the one sample we
                    ;; already recorded.
                    (when (zerop frame)
                      #+TODO (decf (samples-index samples)
                                   (+ sb-sprof::+elements-per-trace-start+
                                      (* sb-sprof::+elements-per-sample+ (1+ i)))))
                    (return)
                :finally (setf (trace-buffer-count buffer) frame)))))))

(defun sigprof-handler/cpu (signal code scp)
  (declare (ignore signal code)
           (optimize speed (space 0))
           (sb-ext:disable-package-locks sb-di::x86-call-context)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type sb-alien:system-area-pointer scp))
  (when-let* ((context *context*)
              (thread  sb-thread:*current-thread*))
    (when (and (if-let ((test (context-thread-test context)))
                 (funcall test thread)
                 t)
               (not recording:*in-critical-recording-code?*))
      (let ((recording:*in-critical-recording-code?* t))
        (sb-thread::with-system-mutex (sb-sprof::*profiler-lock* :without-gcing t) ; TODO is the lock needed? is without-gcing needed?
          ;; Bind `*print-circle*' to avoid touching the circularity
          ;; hash-table when (re-)entering the pretty printer.
          (let ((*print-circle* nil))
            (let ((depth-limit (context-depth-limit context))
                  (trace       (maybe-produce-trace context)))
              (when (not trace)
                ;; TODO warn about trace buffer being too small, count overflows
                (return-from sigprof-handler/cpu))
              (setf (trace-buffer-thread trace) thread
                    (trace-buffer-time   trace) (time:real-time))
              (collect-stacktrace scp depth-limit trace)
              (commit-trace context)))))))
  nil)
