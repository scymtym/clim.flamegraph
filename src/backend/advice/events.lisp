(cl:in-package #:clim.flamegraph.backend.advice)

(defmacro recording-event ((event-var state kind name) &body body)
  (once-only (state)
    `(when-let ((,event-var (produce-event ,state)))
       (setf (event-kind ,event-var) ,kind
             (event-time ,event-var) (time:real-time)
             (event-name ,event-var) ,name)
       ,@body
       (commit-event ,state))))

;;; Ordinary calls, optionally with arguments and return values

(defun note-enter (state name)
  (recording-event (event state :enter name)))

(defun note-enter/args (state name args)
  (recording-event (event state :enter/args name)
    (setf (event-values event) (map 'list (lambda (value)
                                            (if (sb-ext:stack-allocated-p value)
                                                `(:stack-allocated ,(ignore-errors (princ-to-string value)))
                                                value))
                                    args))))

(defun note-leave (state name)
  (recording-event (event state :leave name)))

(defun recording-call (name function &rest args)
  (declare (dynamic-extent args))
  (with-thread-state2 (state) ; TODO bind special so nested calls can avoid thread lookup?
    (progn
      (let ((*recording-state* nil)
            (*thread-state*    nil)) ; TODO flip flag in state?
        (note-enter state name))
      (unwind-protect
           (sb-sys:with-interrupts
             (apply function args))
        (let ((*recording-state* nil)
              (*thread-state*    nil))
          (note-leave state name))))
    (apply function args)))

(defun recording-call/args (name function &rest args)
  ;; (declare (dynamic-extent args))
  (with-thread-state2 (state) ; TODO bind special so nested calls can avoid thread lookup?
    (progn
      (let ((*recording-state* nil)
            (*thread-state*    nil))    ; TODO flip flag in state?
        (note-enter/args state name args))
      (unwind-protect
           (sb-sys:with-interrupts
             (apply function args))
        (let ((*recording-state* nil)
              (*thread-state*    nil))
          (note-leave state name))))
    (apply function args)))

#+later (defun call-and-record/args+values (name function &rest args)
  (with-thread-state (state)
    (note-enter state name args)
    (let ((values))
      (unwind-protect
           (progn
             (setf values (multiple-value-list (apply function args)))
             (values-list values))
        (note-leave state name)         ; values
        ))))

;;; Blocking calls

(defun note-enter/block (state name object)
  (recording-event (event state :enter/block name)
    (setf (event-values event) object)))

(defun note-enter/unblock (state name object)
  (recording-event (event state :enter/unblock name)
    (setf (event-values event) object)))

(defun note-leave/unblock (state name object)
  (recording-event (event state :leave/unblock name)
    (setf (event-values event) object)))

(defun recording-call/block (name function object &rest args)
  (declare (dynamic-extent args))
  (with-thread-state2 (state)
    (progn
      (note-enter/block state name object)
      (unwind-protect
           (apply function object args)
        (note-leave state name)))
    (apply function object args)))

(defun recording-call/unblock (name function object &rest args)
  (declare (dynamic-extent args))
  (with-thread-state2 (state)
    (progn
      (note-enter/unblock state name object)
      (unwind-protect
           (apply function object args)
        (note-leave state name)))
    (apply function object args)))

(defun recording-call/blocking (name function object &rest args)
  (declare (dynamic-extent args))
  (with-thread-state2 (state)
    (progn
      (note-enter/block state name object)
      (unwind-protect
           (apply function object args)
        (note-leave/unblock state name object)))
    (apply function object args)))

;;;

(defun record-name (name &key (recorder (curry #'recording-call/args name)))
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
    (record-name name :recorder (curry #'recording-call/blocking name)))

  (record-name 'sb-ext:process-wait :recorder (curry #'recording-call/blocking 'sb-ext:process-wait))
  (record-name 'sb-impl::waitpid :recorder (curry #'recording-call/blocking 'sb-impl::waitpid))
  (record-name 'sb-impl::get-processes-status-changes)

  (record-name 'sb-thread:condition-wait :recorder (curry #'recording-call/blocking 'sb-thread:condition-wait))
  (record-name 'sb-thread:join-thread :recorder (curry #'recording-call/blocking 'sb-thread:join-thread))

  ; (record-name 'sb-thread:grab-mutex :recorder (curry #'recording-call/block 'sb-thread:grab-mutex))
  ; (record-name 'sb-thread:release-mutex :recorder (curry #'recording-call/unblock 'sb-thread:release-mutex))
  )

(defun record-io ()
  (record-name 'directory :recorder (curry #'recording-call/blocking 'directory))
  (record-name 'open :recorder (curry #'recording-call/blocking 'open))
  (record-name 'close :recorder (curry #'recording-call/blocking 'close))


  (record-name 'sb-bsd-sockets:get-host-by-name     :recorder (curry #'recording-call/blocking 'sb-bsd-sockets:get-host-by-name))
  (record-name 'sb-bsd-sockets:get-host-by-address  :recorder (curry #'recording-call/blocking 'sb-bsd-sockets:get-host-by-address))
  (record-name 'sb-bsd-sockets:get-protocol-by-name :recorder (curry #'recording-call/blocking 'sb-bsd-sockets:get-protocol-by-name))


  #+no (dolist (name '(read-character read-character-no read-line read-sequence))))

(defun record-notable ()
  (dolist (name '(compile intern))
    (record-name name)))
