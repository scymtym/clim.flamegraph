;;;; events.lisp --- Call-like events this source can record.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

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

(defvar *enter-count* (cons 0 0))

(defun note-enter (state name)
  (sb-ext:atomic-incf (car *enter-count*))
  (recording-event (event state :enter name)))

(defun note-enter/args (state name args)
  (declare (dynamic-extent args))
  (sb-ext:atomic-incf (cdr *enter-count*))
  (recording-event (event state :enter/args name)
    (setf (event-values event) (map 'list (lambda (value)
                                            (if (sb-ext:stack-allocated-p value)
                                                `(:stack-allocated ,(ignore-errors (princ-to-string value)))
                                                value))
                                    args))))

(defun note-leave (state name)
  (recording-event (event state :leave name)))

(defvar *call-count* (cons 0 0))

(defun make-recording-call (name)
  (named-lambda recording-call (function &rest args)
    (declare (type function function)
             (dynamic-extent args))
    (sb-ext:atomic-incf (car *call-count*))
    (let ((*context* (list* name *context*)))
      (with-thread-state-and-nesting (state)
        (progn
          (let ((*recording-state* nil)
                (*thread-state*    nil))   ; TODO flip flag in state?
            (note-enter state name))
          (unwind-protect
               (sb-sys:with-interrupts
                 (let ((*in* nil))
                   (apply function args)))
            (let ((*recording-state* nil)
                  (*thread-state*    nil))
              (note-leave state name))))
        (apply function args)))))

(defun make-recording-call/args (name)
  (named-lambda recording-call/args (function &rest args)
    (declare (type function function)
             (dynamic-extent args))
    (sb-ext:atomic-incf (cdr *call-count*))
    (with-thread-state-and-nesting (state)
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
      (apply function args))))

#+later (defun call-and-record/args+values (name function &rest args)
  (with-thread-state-and-nesting (state)
    (note-enter state name args)
    (let ((values))
      (unwind-protect
           (progn
             (setf values (multiple-value-list (apply function args)))
             (values-list values))
        (note-leave state name)         ; values
        ))))

;;; Blocking calls
;;;
;;; There are two flavors:
;;;
;;; 1) `wait-for-SOMETHING' or `call-with-SOMETHING-locked'. In this
;;;    case, the blocking operation corresponds to a single function
;;;    call. We instrument the function with `recording-call/blocking'
;;;    with calls `note-enter/block' and `note-leave/unblock' around
;;;    the function call.
;;;
;;; 2) A pair `lock-SOMETHING' and `unlock-SOMETHING'. In this case,
;;;    the blocking operation corresponds to the region that starts
;;;    when `lock-SOMETHING' returns and ends when `unlock-SOMETHING'
;;;    is called. We instruction the functions with
;;;    `recording-call/block' and `recording-call/unblock'
;;;    respectively.

(defun note-enter/block (state name object)
  (recording-event (event state :enter/block name)
    (setf (event-values event) object)))

(defun note-leave/block (state name object)
  (recording-event (event state :leave/block name)
    (setf (event-values event) object))) ; TODO object not needed

(defun note-enter/unblock (state name object)
  (recording-event (event state :enter/unblock name)
    (setf (event-values event) object)))

(defun note-leave/unblock (state name object)
  (recording-event (event state :leave/unblock name)
    (setf (event-values event) object))) ; TODO object not needed

(defun make-recording-call/block (name)
  (alexandria:named-lambda recording-call/block (function object &rest args)
    (declare (type function function)
             (dynamic-extent args))
    (with-thread-state-and-nesting (state)
      (progn
        (note-enter/block state name object)
        (unwind-protect
             (apply function object args)
          (note-leave/block state name object)))
      (apply function object args))))

(defun make-recording-call/unblock (name block-name)
  (alexandria:named-lambda recording-call/unblock (function object &rest args)
    (declare (type function function)
             (dynamic-extent args))
    (with-thread-state-and-nesting (state)
      (progn
        (note-enter/unblock state name block-name)
        (unwind-protect
             (apply function object args)
          (note-leave state name)))
      (apply function object args))))

(defun make-recording-call/blocking (name)
  (named-lambda recording-call/blocking (function &rest args)
    (declare (type function function)
             (dynamic-extent args))
    (with-thread-state-and-nesting (state)
      (progn
        (note-enter/block state name (first args))
        (unwind-protect
             (apply function args)
          (note-leave/unblock state name (first args)))) ; TODO the object is not needed here
      (apply function args))))

;;;

(defvar *blacklist*
  '(sb-c::allocate-code-object
    sb-c::sap-read-var-integer))

(defun record-name (name &key (recorder (make-recording-call/args name)))
  (when (member name *blacklist* :test #'eq)
    (return-from record-name))
  (sb-int:unencapsulate name 'recorder)
  (sb-int:encapsulate name 'recorder recorder))

(defun unrecord-name (name)
  (when (member name *blacklist* :test #'eq)
    (return-from unrecord-name))
  (sb-int:unencapsulate name 'recorder))

(defmethod record ((thing t)
                   &key (arguments? nil)
                        (recorder   (if arguments?
                                        #'make-recording-call/args
                                        #'make-recording-call)))
  (map nil (lambda (name)
             (record-name name :recorder (funcall recorder name)))
       (thing->names thing)))

(defmethod unrecord ((thing t))
  (map nil #'unrecord-name (thing->names thing)))

(defmethod thing->names ((thing symbol))
  (list thing))

;;; TODO record methods of generic

(defmethod thing->names ((thing function))
  (list (nth-value 2 (function-lambda-expression thing))))

(defmethod thing->names ((thing string))
  (if (ends-with #\* thing)
      (mappend #'thing->names
               (let ((prefix (subseq thing 0 (1- (length thing)))))
                 (remove prefix (list-all-packages)
                         :test-not #'starts-with-subseq
                         :key      #'package-name)))
      (if-let ((package (find-package thing)))
        (thing->names package)
        '())))

(defmethod thing->names ((thing package))
  (let ((result '()))
    (do-symbols (symbol thing)
      (when (eq (symbol-package symbol) thing)
        (when (and (fboundp symbol)
                   (not (macro-function symbol))
                   (not (special-operator-p symbol)))
          (push symbol result))
        #+later (when (fdefinition `(setf ,symbol))
                  (push `(setf symbol) result))))
    result))

;;; Recording for implementation specific blocking calls

(eval-when (:compile-toplevel :execute)
  (when (and (find-symbol "NANOSLEEP-FLOAT"  "SB-UNIX")
             (find-symbol "NANOSLEEP-DOUBLE" "SB-UNIX")
             (find-symbol "NANOSLEEP"        "SB-UNIX")
             (find-symbol "WAITPID"          "SB-IMPL"))
    (pushnew 'unix-nanosleep *features*)))

(defvar *blockers*
  '(#+clim.flamegraph.backend.advice::unix-nanosleep
    (sb-unix::nanosleep-float              . make-recording-call/blocking)
    #+clim.flamegraph.backend.advice::unix-nanosleep
    (sb-unix::nanosleep-double             . make-recording-call/blocking)
    #+clim.flamegraph.backend.advice::unix-nanosleep
    (sb-unix::nanosleep                    . make-recording-call/blocking)

    (sb-ext:process-wait                   . make-recording-call/blocking)
    #+clim.flamegraph.backend.advice::unix-nanosleep
    (sb-impl::waitpid                      . make-recording-call/blocking)
    (sb-impl::get-processes-status-changes . make-recording-call/args)

    (sb-thread:condition-wait              . make-recording-call/blocking)
    (sb-thread:join-thread                 . make-recording-call/blocking)

    (sb-thread:grab-mutex                  . make-recording-call/block)
    (sb-thread:release-mutex               . (make-recording-call/unblock sb-thread:grab-mutex))))

(defmethod record ((thing (eql :blockers)) &key recorder)
  (declare (ignore recorder))
  (loop :for (name . spec) :in *blockers*
        :for (maker . extra-args) = (ensure-list spec)
        :for recorder = (apply maker name extra-args)
        :do (record-name name :recorder recorder))) ; TODO symbol-function?

(defmethod unrecord ((thing (eql :blockers)))
  (map 'nil (compose #'unrecord-name #'car) *blockers*))

;;;

(defun record-io ()
  (record-name 'directory :recorder (make-recording-call/blocking 'directory))
  (record-name 'open :recorder (make-recording-call/blocking 'open))
  (record-name 'close :recorder (make-recording-call/blocking 'close))
  (record-name 'sb-sys:wait-until-fd-usable :recorder (make-recording-call/blocking 'sb-sys:wait-until-fd-usable))

  (record-name 'sb-bsd-sockets:get-host-by-name     :recorder (make-recording-call/blocking 'sb-bsd-sockets:get-host-by-name))
  (record-name 'sb-bsd-sockets:get-host-by-address  :recorder (make-recording-call/blocking 'sb-bsd-sockets:get-host-by-address))
  (record-name 'sb-bsd-sockets:get-protocol-by-name :recorder (make-recording-call/blocking 'sb-bsd-sockets:get-protocol-by-name))


  #+no (dolist (name '(read-character read-character-no read-line read-sequence))))

(defmethod record ((thing (eql :io)) &key recorder)
  (declare (ignore recorder))
  (record-io))

(defmethod unrecord ((thing (eql :io)))
  (warn "TODO"))

;;;

(defun record-notable ()
  (dolist (name '(compile intern))
    (record-name name)))
