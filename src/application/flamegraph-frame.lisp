;;;; flamegraph-frame.lisp --- Frame for inspecting flamegraphs.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.application)

(clim:define-application-frame flamegraph-frame ()
  ((%run  :initarg  :run                  ; TODO temp
          :reader   run)
   (%tree :initarg  :tree
          :reader   tree))
  (:panes
   (history     :application :height           40
                             :scroll-bars      nil)
   (zoom        :slider      :min-value        1
                             :max-value        10
                             :value            1
                             :orientation      :horizontal
                             :label            "Zoom"
                             :show-value-p     t)
   (depth       :slider      :min-value        1
                             :max-value        10
                             :value            1
                             :orientation      :horizontal
                             :label            "Depth"
                             :show-value-p     t)
   (dim-uninteresting :toggle-button :label "Dim uninteresting")
   (function-list flat::flat-pane :model (run clim:*application-frame*))
   (flamegraph    flamegraph::flamegraph-pane :model       (tree clim:*application-frame*)
                                              :width       10
                                              :scroll-bars nil)
   (interactor    :interactor))
  (:layouts
   (:default (clim:vertically ()
               (:fill (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page)
                        ("Function list"
                         (clim:scrolling ()
                           function-list))
                        ("Flamegraph"
                         (clim:vertically ()
                           history
                           (climi::bordering (:thickness 8 :background clim:+gray84+)
                             (clim:horizontally (:spacing 8)
                               zoom
                               depth
                               dim-uninteresting
                               :fill))
                           (:fill (clim:scrolling (:scroll-bars :vertical)
                                    flamegraph))))))
               (clim:make-pane 'clime:box-adjuster-gadget)
               ( 1/16 interactor))))
  (:command-table (flamegraph-frame :inherit-from (flat::command-table)))
  (:menu-bar nil)
  (:pointer-documentation t)
  (:update-instances-on-redefinition t))

(clim:define-command (com-focus :command-table flamegraph-frame
                                :name          t)
    ((node flamegraph::node :gesture :select))
  (let* ((frame      clim:*application-frame*)
         (flamegraph (clim:find-pane-named frame 'flamegraph)))
    (setf (flamegraph::focused flamegraph) (make-instance 'model::standard-tree :root node))))

(defun find-flamegraph-pane (child-sheet)
  (labels ((rec (sheet)
             (cond ((null sheet)
                    nil)
                   ((typep sheet 'flamegraph::flamegraph-pane)
                    sheet)
                   (t
                    (rec (clim:sheet-parent sheet))))))
    (rec child-sheet)))

(clim:define-command (com-reset-focus :command-table flamegraph-frame
                                      :name          t)
    ((background clim:blank-area
                 :gesture (:select
                           :tester ((object)
                                    (when object
                                      (when-let ((flamegraph-pane (find-flamegraph-pane
                                                                   (clim:event-sheet object))))
                                        (not (eq (flamegraph::focused flamegraph-pane)
                                                 (flamegraph::model flamegraph-pane))))))
                           :documentation "Back to root"
                           :pointer-documentation "Back to root"
                           :echo nil)))
  (when-let ((flamegraph (find-flamegraph-pane (clim:event-sheet background))))
    (setf (flamegraph::focused flamegraph) (flamegraph::model flamegraph))))
