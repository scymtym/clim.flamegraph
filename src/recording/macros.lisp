;;;; macros.lisp --- Macros provided by the recording module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.recording)

;;;

(defun call-with-recording (thunk recorder)
  (let ((run (make-run recorder)))
    (setup recorder run)
    (start recorder run)
    (unwind-protect
         (funcall thunk)
      (stop recorder run)
      (teardown recorder run))
    run))

(defmacro with-recording ((recorder) &body body)
  `(call-with-recording (lambda () ,@body) ,recorder))
