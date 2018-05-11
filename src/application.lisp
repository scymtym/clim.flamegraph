;;;; application.lisp --- Application frame for clim.flamegraph.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Frame

(clim:define-application-frame flamegraph ()
  ((traces          :initarg  :traces
                    :accessor traces)
   (selected-traces :accessor selected-traces
                    :initform nil))
  (:panes
   (timeline              timeline-pane)
   (scale                 clim:slider
                          :min-value   -10
                          :max-value   10
                          :value       1
                          :orientation :vertical)
   #+crashes-x-server (separate-threads      toggle-button
                                             :value t)
   (flat                  clim:application-pane
                          :display-function 'display-flat)
   (flamegraph            clim:application-pane
                          :display-function 'display-flame-graph
                          :end-of-line-action :scroll
                          :end-of-page-action :scroll)
   #+no (callgraph             application-pane
                               )
   (interactor            :interactor)
   (pointer-documentation :pointer-documentation))
  (:layouts
   (:default
    (clim:vertically ()
      (clim:scrolling (:scroll-bars :both)
        timeline)
      (:fill (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page)
               ("Flat Profile"
                (clim:scrolling (:scroll-bars :both)
                  flat))
               ("Flamegraph"
                (clim:horizontally ()
                  #+crashes-x-server (vertically ()
                    scale
                    separate-threads)
                  scale
                  (:fill (clim:scrolling (:scroll-bars :both)
                           flamegraph))))
               #+no ("callgraph"
                     (scrolling ()
                       callgraph))))
      (1/8 interactor)
      (1/16 pointer-documentation)))))

(defmethod (setf selected-traces) :after ((new-value t)
                                          (frame     flamegraph))
  (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'flamegraph)))
