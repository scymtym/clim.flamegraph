;;;; application.lisp --- Application frame for clim.flamegraph.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Frame

(defun make-selection-from-traces (traces)
  (let ((start (sb-sprof::samples-start-time traces))
        (end   (sb-sprof::samples-end-time traces)))
    (make-selection 0 (if (and start end)
                          (/ (- end start) internal-time-units-per-second)
                          3))))

(defun scale-changed-callback (depth-limit)
  (lambda (gadget value)
    (let* ((frame      (clim:pane-frame gadget))
           (flamegraph (clim:find-pane-named frame 'flamegraph))
           (state      (state flamegraph)))
      (setf (%depth-limit state) (or depth-limit
                                     (clim:gadget-value
                                      (clim:find-pane-named frame 'depth-limit)))
            (scale        state) value))))

(defun depth-limit-changed-callback (gadget value)
  (let* ((flamegraph (clim:find-pane-named (clim:pane-frame gadget) 'flamegraph))
         (state      (state flamegraph)))
    (setf (depth-limit state) value)))

(clim:define-application-frame flamegraph ()
  ((traces          :initarg  :traces
                    :accessor traces)
   (selected-traces :accessor selected-traces
                    :initform nil))
  (:panes
   (timeline              timeline-pane
                          :selection (let* ((frame     clim:*application-frame*)
                                            (selection (make-selection-from-traces
                                                        (traces frame)))
                                            (interval  (interval selection)))
                                       (setf (selected-traces frame)
                                             (select-traces (traces frame)
                                                            :start-time (start interval)
                                                            :end-time   (end   interval)))
                                       selection))
   (scale                 clim:slider
                          :min-value              0
                          :max-value              10
                          :value                  0
                          :orientation            :horizontal
                          :show-value-p           t
                          :decimal-places         2
                          :drag-callback          (scale-changed-callback 120)
                          :value-changed-callback (scale-changed-callback nil))
   (depth-limit           clim:slider
                          :min-value              0
                          :max-value              1000
                          :value                  100
                          :orientation            :horizontal
                          :show-value-p           t
                          :drag-callback          #'depth-limit-changed-callback
                          :value-changed-callback #'depth-limit-changed-callback)
   #+crashes-x-server (separate-threads      toggle-button
                                             :value t)
   (flat                  clim:application-pane
                          :display-function 'display-flat)
   (flamegraph            flamegraph-pane)
   #+no (callgraph             application-pane
                               )
   (interactor            :interactor))
  (:pointer-documentation t)
  (:layouts
   (:default
    (clim:vertically ()
      (clim:scrolling (:scroll-bars :both)
        timeline)
      (clim:make-pane 'clime:box-adjuster-gadget)
      (:fill (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page)
               ("Flat Profile"
                (clim:scrolling (:scroll-bars :both)
                  flat))
               ("Flamegraph"
                (clim:vertically ()
                  (clim:horizontally ()
                    #+crashes-x-server (vertically ()
                                         scale
                                         separate-threads)
                    (clim:labelling (:label "Zoom")
                      scale)
                    (clim:labelling (:label "Depth Limit")
                      depth-limit))
                  (:fill (clim:scrolling (:scroll-bars :both)
                           flamegraph))))
               #+no ("callgraph"
                     (scrolling ()
                       callgraph))))
      (1/8 interactor))))
  (:default-initargs
   :width 800))

(defmethod clim:layout-frame :after ((frame flamegraph) &optional width height)
  (declare (ignore width height))
  (when-let* ((flamegraph      (clim:find-pane-named frame 'flamegraph))
              (selected-traces (selected-traces frame))
              (selection       (selection (clim:find-pane-named frame 'timeline))))
    (when (null (tree (state flamegraph)))
      (update-flamegraph-traces flamegraph selected-traces selection))))

(defun update-flamegraph-traces (pane selected-traces selection)
  (let ((state      (state pane)))
    (let* ((thread-tree (traces->thread-trees selected-traces))
           (children    (%node-children thread-tree)))
      (loop :for thread :in (hash-table-keys children)
            :unless (selected? thread selection)
            :do (remhash thread children))
      (setf (thread-tree state) thread-tree))

    (setf (tree state) (traces->tree selected-traces))))

(defmethod (setf selected-traces) :after ((new-value t)
                                          (frame     flamegraph))
  ;; TODO flamegraph-pane should do this itself
  (when-let* ((flamegraph      (clim:find-pane-named frame 'flamegraph))
              (selected-traces (selected-traces frame))
              (selection       (selection (clim:find-pane-named frame 'timeline))))
    (update-flamegraph-traces flamegraph selected-traces selection)))
