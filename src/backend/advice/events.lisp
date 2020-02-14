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

(defun recording-call (name function &rest args)
  (declare (dynamic-extent args))
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
     (apply function args))))

(defun recording-call/args (name function &rest args)
  ;; (declare (dynamic-extent args))
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
    (apply function args)))

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

(defun note-enter/block (state name args)
  (declare (dynamic-extent args))
  (recording-event (event state :enter/block name)
    (setf (event-values event) (list (first args)))))

(defun note-enter/unblock (state name object)
  (recording-event (event state :enter/unblock name)
    (setf (event-values event) object)))

(defun note-leave/unblock (state name object)
  (recording-event (event state :leave/unblock name)
    (setf (event-values event) object))) ; TODO object not needed

(defun recording-call/block (name function object &rest args)
  (declare (dynamic-extent args))
  (with-thread-state-and-nesting (state)
    (progn
      (note-enter/block state name object)
      (unwind-protect
           (apply function object args)
        (note-leave state name)))
    (apply function object args)))

(defun recording-call/unblock (name function object &rest args)
  (declare (dynamic-extent args))
  (with-thread-state-and-nesting (state)
    (progn
      (note-enter/unblock state name object)
      (unwind-protect
           (apply function object args)
        (note-leave state name)))
    (apply function object args)))

(defun recording-call/blocking (name function &rest args)
  (declare (dynamic-extent args))
  (with-thread-state-and-nesting (state)
    (progn
      (note-enter/block state name args)
      (unwind-protect
           (apply function args)
        (note-leave/unblock state name (first args)))) ; TODO the object is not needed here
    (apply function args)))

;;;

(defvar *blacklist*
  '(sb-c::allocate-code-object
    sb-c::sap-read-var-integer))

(defun record-name (name &key (recorder (curry #'recording-call/args name)))
  (when (member name *blacklist* :test #'eq)
    (return-from record-name))
  (sb-int:unencapsulate name 'recorder)
  (sb-int:encapsulate name 'recorder recorder))

(defun unrecord-name (name)
  (when (member name *blacklist* :test #'eq)
    (return-from unrecord-name))
  (sb-int:unencapsulate name 'recorder))

(defmethod record ((thing t))
  (map nil #'record-name (thing->names thing)))

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
      (thing->names (find-package thing))))

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

(defvar *blockers*
  '((sb-unix::nanosleep-float              . recording-call/blocking)
    (sb-unix::nanosleep-double             . recording-call/blocking)
    (sb-unix::nanosleep                    . recording-call/blocking)

    (sb-ext:process-wait                   . recording-call/blocking)
    (sb-impl::waitpid                      . recording-call/blocking)
    (sb-impl::get-processes-status-changes . recording-call/args)

    ; (sb-thread:condition-wait              . recording-call/blocking)
    ; (sb-thread:join-thread                 . recording-call/blocking)

    ; (sb-thread:grab-mutex                  . recording-call/block)
    ; (sb-thread:release-mutex               . recording-call/unblock)
    ))

(defmethod record ((thing (eql :blockers)))
  (loop :for (name . recorder) :in *blockers*
        :do (record-name name :recorder (curry (fdefinition recorder) name)))) ; TODO symbol-function?

(defmethod unrecord ((thing (eql :blockers)))
  (map 'nil (compose #'unrecord-name #'car) *blockers*))

;;;

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
