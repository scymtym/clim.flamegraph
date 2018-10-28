;;;; flat.lisp --- Flat profile view.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

(clim:define-presentation-type node ())

(defun format-count-cell (stream count total style)
  (let* ((ratio (/ count total))
         (text  (ecase style
                  (:count
                   (format nil "~:D" count))
                  (:ratio
                   (format nil "~,2,2F" ratio)))))
    (clim:formatting-cell (stream :align-x :right)
      (clim:with-drawing-options (stream :ink (cond ; TODO
                                                ((> ratio .1)  clim:+red+)
                                                ((> ratio .01) clim:+orange+)
                                                (t             clim:+black+)))
        (write-string text stream)))))

(defun display-flat (frame pane)
  (declare (ignore frame))
  (when sb-sprof::*samples*
    (let* ((stream pane)
           (trace-count (sb-sprof::samples-trace-count sb-sprof::*samples*))
           (graph       (sb-sys:without-gcing (sb-sprof::make-call-graph-1 100 #+no most-positive-fixnum #+todo (traces frame))))
           (nodes       (stable-sort (sb-sprof::graph-vertices graph)
                                     (sb-sprof::make-node-comparator
                                      :samples :descending))))
      (clim:formatting-table (stream)
        (dolist (node nodes)
          (clim:formatting-row (stream)
            ;; Name
            (clim:formatting-column (stream)
              (clim:formatting-cell (stream)
                (let ((*print-pretty* nil))
                  (clim:with-output-as-presentation (stream node 'node)
                    (clim:with-drawing-options (stream :ink clim:+red+)
                      (let ((name (princ-to-string  (sb-sprof::node-name node))))
                        (write-string name stream :end (min 80 (length name)))))))))
            ;; Count
            (clim:formatting-column (stream)
              (format-count-cell stream (sb-sprof::node-count node) trace-count :ratio))
            ;; Accrued count
            (clim:formatting-column (stream)
              (format-count-cell stream (sb-sprof::node-accrued-count node) trace-count :ratio))))))))
