(cl:in-package #:clim.flamegraph.application)

;;; `source-configuration-pane'

(defun display-source-configuration (frame pane)
  (declare (ignore frame))
  (let ((configuration (configuration pane)))
    (clim:present configuration (clim:presentation-type-of configuration)
                  :stream pane :single-box t)))

(defclass source-configuration-pane (clim:application-pane)
  ((%configuration :initarg :configuration
                   :reader  configuration))
  (:default-initargs
   :display-function 'display-source-configuration
   :default-view     (make-instance 'configuration-view)))

(defun find-configuration-pane (frame)
  (block nil
    (clim:map-over-sheets
     (lambda (sheet)
       (print sheet *trace-output*)
       (when (typep sheet 'source-configuration-pane)
         (return sheet)))
     (clim:frame-top-level-sheet frame))))

(defun find-configuration (frame)
  (configuration (find-configuration-pane frame)))

;;; `thread-list-pane'

(defun display-thread-list (frame pane)
  (declare (ignore frame))
  (let* ((configuration (configuration pane))
         (threads       (set-difference (bt:all-threads) *threads*))
         (sorted        (sort (copy-list threads) #'string< :key #'bt:thread-name)))
    (clim:with-end-of-line-action (pane :allow)
      (clim:format-items sorted :presentation-type 'non-configured-thread
                                :stream pane :n-columns 1))))

(defclass thread-list-pane (clim:application-pane)
  ((%configuration :initarg :configuration
                   :reader  configuration))
  (:default-initargs
   :display-function 'display-thread-list
   :default-view     (make-instance 'configuration-view)))
