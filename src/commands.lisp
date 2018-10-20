;;;; commands.lisp --- Commands.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Selection and Navigation

(define-flamegraph-command (com-select-trace :name t) ((trace t))
  (setf (selected-traces clim:*application-frame*) (list trace)))

(clim:define-presentation-to-command-translator select-trace
    (trace com-select-trace flamegraph)
    (trace)
  `(,trace))

(define-flamegraph-command (com-zoom-call :name t) ((function call))
  (declare (ignore function)))

(clim:define-presentation-to-command-translator zoom-call
    (call com-zoom-call flamegraph
          :tester ((object) (node-call object))
          :pointer-documentation
          ((object stream)
           (format stream "View ~A and its callees"
                   (sb-sprof::node-name (node-call object)))))
    (function)
  `(,function))

(define-flamegraph-command (com-disassemble :name t) ((function call))
  (declare (ignore function))
  (let ((frame clim:*application-frame*))
    (let ((interval (selection-interval (clim:find-pane-named frame 'timeline))))
      (with-accessors ((start start) (end end)) interval
        (setf (selected-traces frame)
              (select-traces (traces frame) :start-time start :end-time end))))))

(clim:define-presentation-to-command-translator disassemble-call
    (call com-disassemble flamegraph
          :tester ((object) (node-call object))
          :pointer-documentation
          ((object stream)
           (format stream "Disassemble ~A"
                   (sb-sprof::node-name (node-call object)))))
    (function)
  `(,function))

;;; Profiling

(define-flamegraph-command (com-start-recording :name t :keystroke (#\s :control)) ()
  (sb-sprof:start-profiling :threads (sb-thread:list-all-threads))
  ; (describe sb-sprof::*samples* *trace-output*)
  (setf (traces clim:*application-frame*) sb-sprof::*samples*))

(define-flamegraph-command (com-stop-recording :name t :keystroke (#\d :control)) ()
  (sb-sprof:stop-profiling)
  ; (describe sb-sprof::*samples* *trace-output*)
  ; (describe (traces clim:*application-frame*) *trace-output*)
  )
