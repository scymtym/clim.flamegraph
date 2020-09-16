;;;; conversion.lisp --- Converting call events to regions.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.backend.advice)

(defclass aggregation-state ()
  (;; Parameters
   (%min-duration  :initarg  :min-duration
                   :reader   min-duration
                   :initform 0)
   ;; State
   (%thread-states :reader   thread-states
                   :initform (make-hash-table :test #'eq))))

(defun ensure-thread-aggregation-state (aggregation-state thread)
  (ensure-gethash thread (thread-states aggregation-state)
                  (make-instance 'thread-aggregation-state
                                 :thread       thread
                                 :min-duration (min-duration aggregation-state))))

(defun work (source sink)
  (loop :with aggregation-state = (make-instance 'aggregation-state
                                                 :min-duration (floor (min-duration source)
                                                                      1/1000000))
        :for  state             = *recording-state*
        :when state
        :do   (process-state sink source state aggregation-state)
              (when (eq (recording-state-recording? state) :terminating)
                (process-state sink source state aggregation-state)
                (return))
              (sleep .01))) ; TODO

(defun process-state (sink source state aggregation-state)
  (let ((chunk (make-array 128 :adjustable t :fill-pointer 0)))
    (maphash (lambda (thread thread-state)
               (unless (eq thread-state :ignore)
                 (loop :with  t-a-s = (ensure-thread-aggregation-state
                                       aggregation-state thread)
                       :for   event = (maybe-consume-event thread-state)
                       :while event
                       :do    (when-let ((result (process-event event t-a-s)))
                                (vector-push-extend result chunk)))))
             (recording-state-thread-states state))
    (recording:add-chunk source sink chunk)))

;;; Ordinary calls with and without values (nested within one thread)
;;;
;;; The `thread-aggregation-state' keeps a stack of "enter" events for
;;; which the corresponding "leave" event has not been encountered
;;; yet. Each time a "leave" event is encountered, the top "enter"
;;; event is popped off the stack and a region is made based on the
;;; two events.

(defclass thread-aggregation-state ()
  (;; Parameters
   (%thread       :initarg  :thread
                  :reader   thread)
   (%min-duration :initarg  :min-duration
                  :type     non-negative-integer ; TODO time type
                  :reader   min-duration
                  :initform 0)
   ;; State
   (%stack        :reader   stack
                  :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod process-event ((event event) (state thread-aggregation-state))
  (process-event-using-kind (event-kind event) event state))

(macrolet
    ((define-event-handler ((kind) &body body)
       `(defmethod process-event-using-kind ((kind  (eql ,kind))
                                             (event event)
                                             (state thread-aggregation-state))
          (let* ((stack (stack state))
                 (name  (event-name event))
                 (time  (event-time event)))
            (declare (type (and (not simple-vector) (vector t)) stack))
            (macrolet ((pop-region (&key (name 'name))
                         `(let ((index (fill-pointer stack)))
                            (when-let ((top (if (plusp index)
                                                (aref stack (1- index))
                                                (progn
                                                  (warn "~@<Dropping ~A event since there is no corresponding enter event on the stack.~@:>"
                                                        event)
                                                  nil))))
                              (when (eq (model:name top) ,name)
                                (vector-pop stack)
                                (let ((min-duration (min-duration state))
                                      (end-time     time))
                                  (cond ((>= (- end-time (model::%start-time top)) min-duration)
                                         (setf (model:end-time top) end-time)
                                         #+no (when (null (model:children top))
                                                (to-leaf! top))
                                         (when (= index 1) ; TODO could check for root-call-region or similar
                                           top))
                                        (t
                                         (unless (= index 1)
                                           (pop (model:children (aref stack (- index 2)))))
                                         nil)))))))
                       (push-region (inner-call-class root-call-class
                                     &key values (possibly-top? t))
                         `(let* (,@(when possibly-top?
                                     `((top (let ((index (fill-pointer stack)))
                                              (when (plusp index)
                                                (aref stack (1- index)))))))
                                 (new (if ,(if possibly-top?
                                               'top
                                               nil)
                                          ,(if possibly-top?
                                               `(let ((region (make-instance
                                                               ',inner-call-class
                                                               :name       name
                                                               :start-time time
                                                               ,@(case values
                                                                   (:values `(:values (event-values event)))
                                                                   (:object `(:object (event-values event)))))))
                                                  (push region (model:children top))
                                                  region)
                                               nil)
                                          (make-instance
                                           ',root-call-class
                                           :thread     (thread state)
                                           :name       name
                                           :start-time time
                                           ,@(case values
                                               (:values `(:values (event-values event)))
                                               (:object `(:object (event-values event))))))))
                            (vector-push-extend new stack))))
              ,@body)))))

  (define-event-handler (:enter)
    (push-region model::call-region/inner model::call-region/root)
    nil)

  (define-event-handler (:enter/args)
    (push-region model::call-region/inner/values model::call-region/root/values
                 :values :values)
    nil)

  (define-event-handler (:leave)
    (pop-region))

  (define-event-handler (:enter/block)
      ; (fresh-line *trace-output*)
      ; (princ event *trace-output*)
    (push-region model::wait-region/inner model::wait-region/root
                 :values :object)
    nil)

  (define-event-handler (:leave/block)
    (prog1
        (pop-region) ; wait-region
      (push-region model::block-region/inner model::block-region/root
                   :values :object)))

  (define-event-handler (:enter/unblock)
      ; (fresh-line *trace-output*)
      ; (princ event *trace-output*)
    (prog1
        (pop-region :name (event-values event)) ; block region
      (push-region model::call-region/inner model::call-region/root)))

  (define-event-handler (:leave/unblock)
      ; (fresh-line *trace-output*)
      ; (princ event *trace-output*)
    ; (pop-region) ; call region
    (pop-region))) ; wait region

(defmethod to-leaf! ((region model::call-region/inner)) ; TODO this is too expensive
  (change-class region 'model::call-region/leaf))

(defmethod to-leaf! ((region model::call-region/inner/values))
  (change-class region 'model::call-region/leaf/values))

(defmethod to-leaf! ((region model::call-region/root))
  region)

(defmethod to-leaf! ((region model::call-region/root/values))
  region)
