;;;; pane.lisp --- A pane for displaying flamegraphs.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.flamegraph)

(defclass flamegraph-pane (clim:application-pane)
  ((%previous-width :accessor previous-width
                    :initform nil)
   (%model          :initarg  :model
                    ;; :type     model::standard-tree
                    :accessor model)
   (%focused        :initarg  :focused
                    :accessor focused
                    :initform nil))
  (:default-initargs
   :display-function 'display))

(defmethod clim:compose-space ((pane flamegraph-pane) &key width height)
  (declare (ignore width height))
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (clim:space-requirement-components (call-next-method))
    (declare (ignore min-width))
    (clim:make-space-requirement :width      width
                                 :min-width  0
                                 :max-width  max-width
                                 :height     height
                                 :min-height min-height
                                 :max-height max-height)))

(defvar *redisplaying-after-change* nil)

(defmethod clim:note-sheet-region-changed ((sheet flamegraph-pane))
  (let ((previous-width (previous-width sheet))
        (new-width      (clim:bounding-rectangle-width
                         (clim:sheet-region sheet))))
    (when (and (or (not previous-width) (/= previous-width new-width))
               (not *redisplaying-after-change*))
      (setf (previous-width sheet) new-width)
      (let ((*redisplaying-after-change* t))
        (clim:redisplay-frame-pane (clim:pane-frame sheet) sheet)))))

(defun display (frame pane)
  (declare (ignore frame))
  (let* ((tree            (or (focused pane) (model pane)))
         (total-hit-count (model:hit-count (model::root tree)))
         (view            (make-instance 'flamegraph-view
                                         :total-hit-count total-hit-count)))
    (setf (clim:stream-default-view pane) view)
    (clim:present tree 'tree :stream pane)))
