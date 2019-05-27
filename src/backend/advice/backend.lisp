;;;; backend.lisp ---
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.DE>

(cl:in-package #:clim.flamegraph.backend.advice)

(defstruct (event
            (:constructor make-event))
  (kind)
  (name)
  (time)
  (data))

(defconstant +event-ring-buffer-size+ 65536)

(defun make-event-ring-buffer ()
  (coerce (loop :repeat +event-ring-buffer-size+ :collect (make-event))
          `(simple-array event (,+event-ring-buffer-size+))))

(defstruct (thread-state
            (:constructor make-thread-state ())
            (:copier nil)
            (:predicate nil))
  (recording? t                        :type boolean)
  (events     (make-event-ring-buffer) :type (simple-array t (#.+event-ring-buffer-size+)) :read-only t)
  (read-head  0                        :type array-index)
  (write-head 0                        :type array-index))

(defun produce-event (thread-state)
  (let ((write-head (thread-state-write-head thread-state)))
    (aref (thread-state-events thread-state) (mod write-head +event-ring-buffer-size+)) ; TODO magic number
    ))

(defun maybe-produce-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    (when (< (- write-head read-head) +event-ring-buffer-size+)
      (produce-event thread-state))))

(defun commit-event (thread-state)
  (incf (thread-state-write-head thread-state)))

(defun consume-event (thread-state)
  (let ((read-head (thread-state-read-head thread-state)))
    (prog1
        (aref (thread-state-events thread-state) (mod read-head +event-ring-buffer-size+))
      (setf (thread-state-read-head thread-state) (1+ read-head)))))

(defun maybe-consume-event (thread-state)
  (let ((read-head  (thread-state-read-head thread-state))
        (write-head (thread-state-write-head thread-state)))
    (when (< read-head write-head)
      (consume-event thread-state))))

(defstruct (recording-state
            (:constructor make-recording-state ()))
  (recording?    nil                                          :type boolean)
  (thread-states (make-hash-table :test #'eq :synchronized t) :type hash-table :read-only t))

;;;

(defvar *recording-state* nil)

(defvar *thread-state* nil)

(declaim (inline ensure-thread-state))
(defun ensure-thread-state (recording-state
                            &optional (thread (bt:current-thread)))
  (let ((table (recording-state-thread-states recording-state)))
    (ensure-gethash thread table (make-thread-state))))

(defmacro with-recording-state ((state-var) &body body)
  `(#+sbcl sb-sys:without-interrupts #-sbcl progn
    (when-let ((,state-var *recording-state*))
      (when (recording-state-recording? ,state-var)
        (let ((*recording-state* nil))
          ,@body)))))

(defmacro with-thread-state ((state-var &optional thread) &body body)
  (alexandria:with-gensyms (recording-state)
    `(with-recording-state (,recording-state)
       (let ((,state-var (ensure-thread-state
                          ,recording-state ,@(when thread `(,thread)))))
         (when (thread-state-recording? ,state-var)
           ,@body)))))

(defmacro with-recording-state2 ((state-var) recording-body normal-body)
  `(#+sbcl sb-sys:without-interrupts #-sbcl progn
    (let ((,state-var *recording-state*))
      (if (and ,state-var (recording-state-recording? ,state-var))
          (#+sbcl sb-sys:allow-with-interrupts #-sbcl progn
           ,recording-body)
          (#+sbcl sb-sys:with-local-interrupts #-sbcl progn ; TODO can we not disable interrupts when not recording?
            ,normal-body)))))

(defmacro with-thread-state2 ((state-var &optional thread)
                              recording-body normal-body)
  (alexandria:with-gensyms
      (recording-state recording-thunk normal-thunk with-state-thunk)
    `(labels ((,recording-thunk (,state-var)
                ,recording-body)
              (,normal-thunk ()
                ,normal-body)
              (,with-state-thunk (,state-var)
                (if (thread-state-recording? ,state-var)
                    (,recording-thunk ,state-var)
                    (,normal-thunk))))
       (with-recording-state2 (,recording-state)
         (if-let ((,state-var *thread-state*))
           (,with-state-thunk ,state-var)
           (let* ((,state-var     (ensure-thread-state
                                   ,recording-state ,@(when thread `(,thread))))
                  (*thread-state* ,state-var))
             (,with-state-thunk ,state-var)))
         (,normal-thunk)))))

(defun call-and-record2 (name function &rest args)
  ;; (declare (dynamic-extent args))
  (with-thread-state2 (state) ; TODO bind special so nested calls can avoid thread lookup?
    (progn
      (let ((*recording-state* nil)
            (*thread-state*    nil)) ; TODO flip flag in state?
        (note-enter state name args))
      (unwind-protect
           (sb-sys:with-interrupts
             (apply function args))
        (let ((*recording-state* nil)
              (*thread-state*    nil))
          (note-leave state name))))
    (apply function args)))

;;;



(defun note-enter (state name values)
  (when-let ((event (produce-event state)))
    (setf (event-kind event) :enter
          (event-time event) (time:real-time)
          (event-name event) name
          (event-data event) (map 'list (lambda (value)
                                          (if (sb-ext:stack-allocated-p value)
                                              `(:stack-allocated ,(ignore-errors (princ-to-string value)))
                                              value))
                                  values))
    (commit-event state)))

(defun note-leave (state name)
  (when-let ((event (produce-event state)))
    (setf (event-kind event) :leave
          (event-time event) (time:real-time)
          (event-name event) name)
    (commit-event state)))

(defun call-and-record (name function &rest args)
  (with-thread-state (state)
    (note-enter state name args)
    (let ((values))
      (unwind-protect
           (progn
             (setf values (multiple-value-list (apply function args)))
             (values-list values))
        (note-leave state name)              ; values
        ))))

#+later (defun note-block (name lock)
  (with-thread-state (state)
    (let* ((stack (thread-state-stack state))
           (top   (let ((index (fill-pointer stack)))
                    (when (plusp index)
                      (aref stack (1- index)))))
           (new   (make-instance 'model::wait-region
                                 :name       name
                                 :start-time (time:real-time)
                                 :lock       lock)))
      (if top
          (push new (model:children top))
          (vector-push-extend new (thread-state-roots state)))
      (vector-push-extend new stack))))

#+later (defun call-and-record/block (name function &rest args)
  (note-block name (first args))
  (unwind-protect
       (apply function args)
    (note-leave state name)))

#+later (defun note-unblock (name lock)
  (with-thread-state (state)
    (let* ((stack (thread-state-stack state))
           (top   (let ((index (fill-pointer stack)))
                    (when (plusp index)
                      (aref stack (1- index)))))
           (new   (make-instance 'model::wait-region
                                 :name       name
                                 :start-time (time:real-time)
                                 :lock       lock)))
      (if top
          (push new (model:children top))
          (vector-push-extend new (thread-state-roots state)))
      (vector-push-extend new stack))))

#+later (defun call-and-record/unblock (name function &rest args)
  (note-unblock name (first args))
  (unwind-protect
       (apply function args)
    (note-leave name)))

(defun record-name (name &key (recorder (curry #'call-and-record2 name)))
  (print name)
  (sb-int:unencapsulate name 'recorder)
  (sb-int:encapsulate name 'recorder recorder))

(defmethod record ((thing symbol))
  (record-name thing))

(defmethod record ((thing function))
  (let ((name (nth-value 2 (function-lambda-expression thing))))
    (record-name name)))

(defmethod record ((thing string))
  (record (find-package thing)))

(defmethod record ((thing package))
  (do-symbols (symbol thing)
    (when (eq (symbol-package symbol) thing)
      (when (fboundp symbol)
        (record-name symbol))
      #+later (when (fdefinition `(setf ,symbol))
                (record-name symbol)))))

;;;

(defun record-blockers ()
  (dolist (name '(sb-unix::nanosleep-float sb-unix::nanosleep-double sb-unix::nanosleep))
    (record-name name :recorder (curry #'call-and-record/block name)))

  (record-name 'sb-ext:process-wait :recorder (curry #'call-and-record/block 'sb-ext:process-wait))
  (record-name 'sb-impl::waitpid :recorder (curry #'call-and-record/block 'sb-impl::waitpid))
  (record-name 'sb-impl::get-processes-status-changes)


  (record-name 'sb-thread:condition-wait :recorder (curry #'call-and-record/block 'sb-thread:condition-wait))
  (record-name 'sb-thread:join-thread :recorder (curry #'call-and-record/block 'sb-thread:join-thread))

  (record-name 'sb-thread:grab-mutex :recorder (curry #'call-and-record/block 'sb-thread:grab-mutex))
  (record-name 'sb-thread:release-mutex :recorder (curry #'call-and-record/block 'sb-thread:release-mutex)))

(defun record-io ()
  (dolist (name '(read-character read-character-no read-line read-sequence))))

(defun record-notable ()
  (dolist (name '(compile intern))
    (record-name name)))

;;; Events



(defmethod model::time :around ((object standard-event)) ; TODO temp
  (/ (call-next-method) 1000000))

(defun note-global-event (name &rest properties &key &allow-other-keys)
  (print (list :note-global-event name properties) *trace-output*)
  (with-thread-state (state :global)
    (let ((new (make-instance 'standard-event
                              :name       name
                              :time       (time:real-time)
                              :properties properties)))
      (vector-push-extend new (thread-state-roots state)))))
