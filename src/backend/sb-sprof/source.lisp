;;;; source.lisp --- Source implementation provided by the backend.sb-sprof module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.sb-sprof)

(defvar *context*)

;;;

(defclass source ()
  ((%sample-interval   :initarg  :sample-interval
                       :reader   sample-interval)
   (%trace-depth-limit :initarg  :trace-depth-limit
                       :reader   trace-depth-limit
                       :initform 1024)
   ;;
   (%run               :accessor run)
   (%thread            :accessor thread)))

(defmethod recording:setup ((source source) (run t))
  (setf *context* (make-context :depth-limit (trace-depth-limit source)))
  (setf (run    source) run
        (thread source) (bt:make-thread (curry #'work run source)
                                             :name "sprof source worker"))
  ;;
  (sb-sys:enable-interrupt
   sb-unix:sigprof #'sigprof-handler/cpu :synchronous t))

(defmethod recording:start ((source source) (run t))
  (multiple-value-bind (secs usecs) ; TODO separate function
      (multiple-value-bind (secs rest)
          (truncate (sample-interval source))
        (values secs (truncate (* rest 1000000))))
    (sb-unix:unix-setitimer :profile secs usecs secs usecs)))

(defmethod recording:stop ((source source) (run t))
  (sb-unix:unix-setitimer :profile 0 0 0 0))

(defmethod recording:teardown ((source source) (run t))
  ; (sb-sys:enable-interrupt sb-unix:sigprof :default)

  (setf *context* nil) ; TODO put a condition variable into the context
  (bt:join-thread (thread source)))

;;; Worker

(defun work (run source)
  (loop :for context = *context* ; TODO termination
        :while context
        :for trace-buffer = (maybe-consume-trace context)
        :when trace-buffer
        :do (recording:add-chunk source run (list (buffer->trace trace-buffer)))
        :do (sleep .01)))

(defun buffer->trace (buffer)
  (make-instance 'model::standard-trace
                 :thread  (trace-buffer-thread buffer)
                 :time    (trace-buffer-time buffer)
                 :samples (when (plusp (trace-buffer-count buffer))
                            (loop :with samples = (trace-buffer-samples buffer)
                                  :for i :downfrom (* 2 (trace-buffer-count buffer)) :to 0 :by 2
                                  :collect (make-instance 'model::standard-sample
                                                          :name (info->name (aref samples i)))))))

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
      (t
       (coerce info 'string)))))

;;; Signal handler

(declaim (inline collect-stacktrace))
(defun collect-stacktrace (scp depth-limit buffer)
  (sb-alien:with-alien ((scp (* sb-sys:os-context-t) :local scp))
    (let* ((pc-ptr (sb-vm:context-pc scp))
           (fp     (sb-vm::context-register scp #.sb-vm::ebp-offset)))
      ;; Foreign code might not have a useful frame pointer in
      ;; ebp/rbp, so make sure it looks reasonable before
      ;; walking the stack
      (unless (sb-di::control-stack-pointer-valid-p (sb-sys:int-sap fp))
        (return-from collect-stacktrace nil))
      (let ((fp (sb-sys:int-sap fp))    ; TODO put into loop?
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
        ;; (setf (trace-buffer-thread buffer) thread)
        (loop :with samples = (trace-buffer-samples buffer)
              :for frame :of-type array-index :below depth-limit
              :for i :from 0 :by 2
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
                   #+todo (decf (samples-index samples)
                                (+ sb-sprof::+elements-per-trace-start+
                                   (* sb-sprof::+elements-per-sample+ (1+ i)))))
                 (return)
              :finally (setf (trace-buffer-count buffer) frame))))))

(defun sigprof-handler/cpu (signal code scp)
  (declare (ignore signal code) (optimize speed (space 0))
           (sb-ext:disable-package-locks sb-di::x86-call-context)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type sb-alien:system-area-pointer scp))
  (when-let* ((context *context*)
              (thread  sb-thread:*current-thread*))
    (when (and t
               #+todo (profiled-thread-p self)
               (not recording:*in-critical-recording-code?*))
      (let ((recording:*in-critical-recording-code?* t))
        (sb-thread::with-system-mutex (sb-sprof::*profiler-lock* :without-gcing t) ; TODO is the lock needed? is without-gcing needed?
          (let ((depth-limit (context-depth-limit context))
                (trace       (maybe-produce-trace context)))
            (when (not trace)
              ;; TODO warn about trace buffer being too small
              (return-from sigprof-handler/cpu))
            (setf (trace-buffer-thread trace) thread
                  (trace-buffer-time   trace) (time:real-time))
            (collect-stacktrace scp depth-limit trace)
            (commit-trace context))))))
  nil)
