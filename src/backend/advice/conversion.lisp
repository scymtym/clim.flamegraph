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
                   :do    (when-let ((result (process-event event t-a-s)))
                            (recording:add-chunk source run (list result)))))
           (recording-state-thread-states state)))

(defmethod process-event ((event event) (state thread-aggregation-state))
  (process-event-using-kind (event-kind event) event state))

;;; Ordinary calls with and without values

(macrolet
    ((define-enter-handler (kind call-class root-call-class values)
       `(defmethod process-event-using-kind ((kind  (eql ,kind))
                                             (event event)
                                             (state thread-aggregation-state))
          (let* ((stack  (stack state))
                 (top    (let ((index (fill-pointer stack)))
                           (when (plusp index)
                             (aref stack (1- index)))))
                 (new    (if top
                             (let ((region (make-instance ',call-class
                                                          :name       (event-name  event)
                                                          :start-time (event-time  event)
                                                          ,@(case values
                                                              (:values `(:values (event-values event)))
                                                              (:object `(:object (event-values event)))))))
                               (push region (model:children top))
                               region)
                             (make-instance ',root-call-class
                                            :thread     (thread state)
                                            :name       (event-name event)
                                            :start-time (event-time event)
                                            ,@(case values
                                                (:values `(:values (event-values event)))
                                                (:object `(:object (event-values event))))))))
            (vector-push-extend new stack)
            nil))))

  (define-enter-handler :enter
    model::call-region/inner model::call-region/root nil)
  (define-enter-handler :enter/args
    model::call-region/inner/values model::call-region/root/values :values)
  (define-enter-handler :enter/block
    model::wait-region/inner model::wait-region/root :object))

(defmethod process-event-using-kind ((kind  (eql :leave))
                                     (event event)
                                     (state thread-aggregation-state))
  (when-let* ((stack (stack state))
              (index (fill-pointer stack))
              (top   (when (plusp index)
                       (aref stack (1- index)))))
    (when (and top (eq (model:name top) (event-name event)))
      (setf (model:end-time top) (event-time event))
      (vector-pop stack)
      (when (= index 1) ; TODO could check for root-call-region or similar
        top))))

(defmethod process-event-using-kind ((kind  (eql :leave/unblock))
                                     (event event)
                                     (state thread-aggregation-state))
  (when-let* ((stack (stack state))
              (index (fill-pointer stack))
              (top   (when (plusp index)
                       (aref stack (1- index)))))
    (when (and top (eq (model:name top) (event-name event)))
      (setf (model:end-time top) (event-time event))
      (vector-pop stack)
      (when (= index 1) ; TODO could check for root-call-region or similar
        top))))
