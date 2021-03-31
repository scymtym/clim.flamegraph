(cl:in-package #:clim.flamegraph.application)

(defclass recording-status-pane (clim:application-pane)
  ((%run :initarg  :run
         :accessor run
         :initform nil))
  (:default-initargs
   :display-function 'display-recording-status))

(defmethod (setf run) :after ((new-value t) (object recording-status-pane))
  (clim:redisplay-frame-pane (clim:pane-frame object) object))

(defun display-recording-status (frame pane)
  (declare (ignore frame))
  (clim:present (run pane) 'recording-process :stream pane))

(clim:define-presentation-type recording-process ()
  :inherit-from t)

(clim:define-presentation-method clim:present ((object t)
                                               (type   recording-process)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (if object
      (format stream "~:D trace~:P" (length (recording::data object)))
      (clim:with-drawing-options (stream :text-face :italic :ink clim:+gray20+)
        (write-string "Not recording" stream))))
