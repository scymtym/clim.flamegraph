(cl:in-package #:clim.flamegraph.backend.advice)

(defclass thread-aggregation-state ()
  ((%thread :initarg  :thread
            :reader   thread)
   (%stack  :reader   stack
            :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass aggregation-state ()
  ((%thread-states :reader   thread-states
                   :initform (make-hash-table :test #'eq))))

(defun ensure-thread-aggregation-state (aggregation-state thread)
  (ensure-gethash thread (thread-states aggregation-state)
                  (make-instance 'thread-aggregation-state :thread thread)))

(defun work (run source)
  (loop :with aggregation-state = (make-instance 'aggregation-state)
        :for  state             = *recording-state*
        :when state
        :do   (process-state run source state aggregation-state)
              (when (eq (recording-state-recording? state) :terminating)
                (process-state run source state aggregation-state)
                (return))
              (sleep .01)))

(defun process-state (run source state aggregation-state)
  (maphash (lambda (thread thread-state)
             (loop :with  t-a-s = (ensure-thread-aggregation-state aggregation-state thread)
                   :for   event = (maybe-consume-event thread-state)
                   :while event
                   :do    (case (event-kind event)
                            (:enter (process-enter-event event t-a-s))
                            (:leave (when-let ((root (process-leave-event event t-a-s)))
                                      (recording:add-chunk source run (list root)))))))
           (recording-state-thread-states state)))

(defun process-enter-event (event state)
  (let* ((stack  (stack state))
         (top    (let ((index (fill-pointer stack)))
                   (when (plusp index)
                     (aref stack (1- index)))))
         (new    (if top
                     (let ((region (make-instance 'model::call-region
                                                  :name       (event-name event)
                                                  :start-time (event-time event)
                                                  :values     (event-data event))))
                       (push region (model:children top))
                       region)
                     (make-instance 'model::root-call-region
                                    :thread     (thread state)
                                    :name       (event-name event)
                                    :start-time (event-time event)
                                    :values     (event-data event)))))
    (vector-push-extend new stack)))

(defun process-leave-event (event state)
  (when-let* ((stack (stack state))
              (index (fill-pointer stack))
              (top   (when (plusp index)
                       (aref stack (1- index)))))
    (when (and top (eq (model:name top) (event-name event)))
      (setf (model:end-time top) (event-time event))
      (vector-pop stack)
      (when (= index 1) ; TODO could check for root-call-region or similar
        top))))
