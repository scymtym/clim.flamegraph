(cl:in-package #:clim.flamegraph.backend.advice)

(defun real-time ()
  (multiple-value-bind (seconds microseconds)
      (sb-unix::get-time-of-day)
    (+ (* 1000000 seconds) microseconds)))

(defconstant time-units-per-second
  1000000)

(defstruct (thread-state
            (:constructor make-thread-state ())
            (:copier nil)
            (:predicate nil))
  (recording? t                                            :type boolean)
  (roots      (make-array 0 :adjustable t :fill-pointer 0) :type (array t 1) :read-only t)
  (stack      (make-array 0 :adjustable t :fill-pointer 0) :type (array t 1) :read-only t))

(defvar *recording-state* nil)

(defvar *last-state*)

(defun detach! (recording-state)
  (sb-ext:gc :full t)
  (let ((result (make-hash-table :test #'eq))
        (seen   (make-hash-table :test #'eq)))
    (maphash (lambda (thread state)
               (map nil (named-lambda rec (node)
                          (reinitialize-instance node :name (let ((name (model:name node)))
                                                              (typecase name
                                                                (symbol (ensure-gethash
                                                                         name seen
                                                                         (make-instance 'model::qualified-name
                                                                                        :container (package-name (symbol-package name))
                                                                                        :name      (symbol-name name))))
                                                                (t (ensure-gethash
                                                                    name seen
                                                                    (princ-to-string name))))))
                          (when (compute-applicable-methods #'model::values* (list node))
                            (reinitialize-instance node :values (map 'list (lambda (v)
                                                                             (ensure-gethash
                                                                              v seen
                                                                              (let ((*print-level* 3) (*print-length* 5))
                                                                                (princ-to-string v))))
                                                                     (model::values* node))))
                          (when (compute-applicable-methods #'model::lock (list node))
                            (reinitialize-instance node :lock (let ((lock (model::lock node)))
                                                                (ensure-gethash
                                                                 lock seen
                                                                 (let ((*print-level* 3) (*print-length* 5))
                                                                   (princ-to-string lock))))))
                          (map nil #'rec (model:children node)))
                    (thread-state-roots state))
               (setf (gethash (princ-to-string thread) result) state))
             recording-state)
    (sb-ext:gc :full t)
    result))

(defun call-with-recording (thunk)
  (let ((state (make-hash-table :test #'eq :synchronized t)))
    (setf *recording-state* state)
    (unwind-protect
         (funcall thunk)
      (setf *last-state* state)
      (setf *recording-state* nil))
    (detach! state)))

(defmacro with-recording (() &body body)
  `(call-with-recording (lambda () ,@body)))

(defmacro with-recording-state ((state-var) &body body)
  `(#+sbcl sb-sys:without-interrupts #-sbcl progn
    (when-let ((,state-var *recording-state*))
      (let ((*recording-state* nil))
        ,@body))))

(defun note-enter (name values)
  (with-recording-state (state)
    (let* ((thread-state (ensure-gethash (bt:current-thread) state (make-thread-state)))
           (stack        (thread-state-stack thread-state))
           (top          (let ((index (fill-pointer stack)))
                           (when (plusp index)
                             (aref stack (1- index)))))
           (values       (map 'list (lambda (value)
                                      (if (sb-ext:stack-allocated-p value)
                                          `(:stack-allocated ,(ignore-errors (princ-to-string value)))
                                          value))
                              values))
           (new          (make-instance 'model::call-region
                                        :name       name
                                        :start-time (real-time)
                                        :values     (map 'list #'princ-to-string values))))
      (if top
          (push new (model:children top))
          (vector-push-extend new (thread-state-roots thread-state)))
      (vector-push-extend new stack))))

(defun note-leave (name)
  (with-recording-state (state)
    (when-let* ((thread-state (gethash (bt:current-thread) state))
                (stack        (thread-state-stack thread-state))
                (top          (let ((index (fill-pointer stack)))
                                (when (plusp index)
                                  (aref stack (1- index))))))
      (when (and top (eq (model:name top) name))
        (setf (model:end-time top) (real-time))
        (vector-pop stack)))))

(defun call-and-record (name function &rest args)
  (note-enter name args)
  (let ((values))
    (unwind-protect
         (progn
           (setf values (multiple-value-list (apply function args)))
           (values-list values))
      (note-leave name)              ; values
      )))

(defun note-block (name lock)
  (with-recording-state (state)
    (let* ((thread-state (ensure-gethash (bt:current-thread) state (make-thread-state)))
           (stack        (thread-state-stack thread-state))
           (top          (let ((index (fill-pointer stack)))
                           (when (plusp index)
                             (aref stack (1- index)))))
           (new          (make-instance 'model::wait-region
                                        :name       name
                                        :start-time (real-time)
                                        :lock       lock)))
      (if top
          (push new (model:children top))
          (vector-push-extend new (thread-state-roots thread-state)))
      (vector-push-extend new stack))))

(defun call-and-record/block (name function &rest args)
  (note-block name (first args))
  (unwind-protect
       (apply function args)
    (note-leave name)))

(defun note-unblock (name lock)
  (with-recording-state (state)
    (let* ((thread-state (ensure-gethash (bt:current-thread) state (make-thread-state)))
           (stack        (thread-state-stack thread-state))
           (top          (let ((index (fill-pointer stack)))
                           (when (plusp index)
                             (aref stack (1- index)))))
           (new          (make-instance 'model::wait-region
                                        :name       name
                                        :start-time (real-time)
                                        :lock       lock)))
      (if top
          (push new (model:children top))
          (vector-push-extend new (thread-state-roots thread-state)))
      (vector-push-extend new stack))))

(defun call-and-record/unblock (name function &rest args)
  (note-unblock name (first args))
  (unwind-protect
       (apply function args)
    (note-leave name)))

(defun record-name (name &key (recorder (curry #'call-and-record name)))
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

(defclass standard-event (model:name-mixin
                          model::temporal-point-mixin
                          print-items:print-items-mixin)
  ((%properties :initarg :properties
                :reader  model::properties)))

(defmethod model::time :around ((object standard-event)) ; TODO temp
  (/ (call-next-method) 1000000))

(defun note-global-event (name &rest properties &key &allow-other-keys)
  (print (list :note-global-event name properties) *trace-output*)
  (with-recording-state (state)
    (let* ((thread-state (ensure-gethash :global state (make-thread-state)))
           (new          (make-instance 'standard-event
                                        :name       name
                                        :time       (real-time)
                                        :properties properties)))
      (vector-push-extend new (thread-state-roots thread-state)))))
