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

(define-flamegraph-command (com-reset-view :name t) ()
  (let ((state (state (clim:find-pane-named clim:*application-frame* 'flamegraph))))
    (setf (%root state) nil
          (scale state) 0)))

(clim:define-presentation-to-command-translator blank-area->reset-view
    (clim:blank-area com-reset-view flamegraph
     :gesture :select
     :tester ((object window)
              (typep window 'flamegraph-pane))
     :documentation
     ((stream)
      (format stream "Show whole flamegraph and reset zoom")))
    (blank-area)
  '())

(define-flamegraph-command (com-zoom-call :name t) ((function call))
  (let ((state (state (clim:find-pane-named clim:*application-frame* 'flamegraph))))
    (setf (%root state) function
          (scale state) 0)
    ;; TODO hack
    (setf (clim:gadget-value (clim:find-pane-named clim:*application-frame* 'scale)) 0)))

(clim:define-presentation-to-command-translator zoom-call
    (call com-zoom-call flamegraph
          :gesture :select
          :tester  ((object) (node-call object))
          :pointer-documentation
          ((object stream)
           (format stream "Focus view on ~A and its callees"
                   (node-name object))))
    (function)
  `(,function))

;;; Disassemble

(define-flamegraph-command (com-disassemble :name t) ((function call))
  (declare (ignore function))
  (let ((frame clim:*application-frame*))
    (let ((interval (selection-interval (clim:find-pane-named frame 'timeline))))
      (with-accessors ((start start) (end end)) interval
        (setf (selected-traces frame)
              (select-traces (traces frame) :start-time start :end-time end))))))

#+no (clim:define-presentation-to-command-translator disassemble-call
    (call com-disassemble flamegraph
          :tester ((object) (node-call object))
          :pointer-documentation
          ((object stream)
           (format stream "Disassemble ~A"
                   (node-name object))))
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

;;; Exporting

(define-flamegraph-command (com-export-flamegraph-image :name t)
    ((filename pathname :prompt "Filename"))
  (let* ((frame  clim:*application-frame*)
         (pane   (clim:find-pane-named frame 'flamegraph))
         (state  (state pane))
         (width  (clim:bounding-rectangle-max-x pane))
         (height (clim:bounding-rectangle-max-y pane)))
    (mcclim-raster-image:with-output-to-raster-image-file
        (stream filename :width width :height height)
      (present-flamegraph state stream))))
