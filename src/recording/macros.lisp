;;;; macros.lisp --- Macros provided by the recording module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

;;;

(defun call-with-recording-into (thunk run recorder)
  (setup recorder run)
  (start recorder run)
  (unwind-protect
       (funcall thunk)
    (stop recorder run)
    (teardown recorder run)))

(defun call-with-recording (thunk recorder)
  (let ((run (make-run recorder)))
    (call-with-recording-into thunk run recorder)
    run))

(defmacro with-recording ((recorder) &body body)
  "Execute BODY with RECORDER recording into a fresh run.

If the execution of BODY completes normally, the return the run."
  `(call-with-recording (lambda () ,@body) ,recorder))
