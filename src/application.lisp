;;;; application.lisp --- Application frame for clim.flamegraph.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Frame

(defun scale-changed-callback (depth-limit)
  (lambda (gadget value)
    (let* ((flamegraph (clim:find-pane-named (clim:pane-frame gadget) 'flamegraph))
           (state      (state flamegraph)))
      (setf (depth-limit state) depth-limit
            (scale       state) value))))

(clim:define-application-frame flamegraph ()
  ((traces          :initarg  :traces
                    :accessor traces)
   (selected-traces :accessor selected-traces
                    :initform nil))
  (:panes
   (timeline              timeline-pane)
   (scale                 clim:slider
                          :min-value              0
                          :max-value              10
                          :value                  0
                          :orientation            :horizontal
                          :show-value-p           t
                          :drag-callback          (scale-changed-callback 120)
                          :value-changed-callback (scale-changed-callback nil))
   #+crashes-x-server (separate-threads      toggle-button
                                             :value t)
   (flat                  clim:application-pane
                          :display-function 'display-flat)
   (flamegraph            (clim:make-pane 'flamegraph-pane))
   #+no (callgraph             application-pane
                               )
   (interactor            :interactor)
   (pointer-documentation :pointer-documentation))
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
                      scale))
                  (:fill (clim:scrolling (:scroll-bars :both)
                           flamegraph))))
               #+no ("callgraph"
                     (scrolling ()
                       callgraph))))
      (1/8 interactor)
      (1/16 pointer-documentation)))))

(defmethod (setf selected-traces) :after ((new-value t)
                                          (frame     flamegraph))
  (setf (tree (state (clim:find-pane-named frame 'flamegraph))) (traces->tree (selected-traces frame))) ; TODO flamegraph-pane should do this itself
  ; (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'flamegraph))
  )
