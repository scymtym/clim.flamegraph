(cl:defpackage #:clim.flamegraph.examples.recording
  (:use
   #:cl)

  (:local-nicknames
   (#:model     #:clim.flamegraph.model)
   (#:recording #:clim.flamegraph.recording))

  (:export
   #:call-with-recording
   #:with-recording))

(cl:in-package #:clim.flamegraph.examples.recording)

(defun call-with-recording (thunk &key specifications
                                       record-blockers?
                                       record-io?
                                       include-recording-threads?
                                       include-recording-functions?
                                       (sample-rate .01)
                                       depth-limit
                                       min-duration)
  (let* ((specifications (append (when record-blockers?
                                   '(:blockers))
                                 (when record-io?
                                   '(:io))
                                 specifications))
         (sources        (list (make-instance 'clim.flamegraph.backend.advice::source
                                              :specification specifications
                                              :sample-rate   sample-rate
                                              :depth-limit   depth-limit
                                              :min-duration  (or min-duration 0))
                               (make-instance 'clim.flamegraph.backend.sb-sprof::source
                                              ;; :thread-test (lambda (thread) ...)
                                              :name-test (lambda (name)
                                                           (not (and (symbolp name)
                                                                     (search "SWANK" (package-name (symbol-package name)))))))
                               #+sbcl (make-instance 'clim.flamegraph.backend.sb-memory::source)))
         (recorder       (recording:make-recorder :standard :sources sources))
         (run            (recording:make-run recorder)))
    (unwind-protect
         (recording:call-with-recording-into thunk run recorder)
      (let ((result (make-instance 'model::standard-run :traces (recording::data run))))
        ;; (clouseau:inspect result :new-process t)
        (when (boundp '*frame*)
          (clim.flamegraph.application::add-run
           (symbol-value '*frame*) result))
        result))))

(defmacro with-recording ((&rest args &key &allow-other-keys) &body body)
  `(call-with-recording (lambda () ,@body) ,@args))

(defvar *frame* (clim.flamegraph.application:launch :new-process t))

#+example (with-recording (:record-blockers? t)
            (loop :repeat 10 :do (sleep 1)))
