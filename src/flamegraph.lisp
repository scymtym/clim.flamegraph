;;;; flamegraph.lisp --- Flamegraph view and presentation.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Presentation types

(defun name+depth+position->color (name depth position)
  (declare (ignore depth position))
  (symbol-color name))

(clim:define-presentation-type call
    (&optional (depth 0) (position 0) (total-count 1)))

(clim:define-presentation-method clim:present ((object node)
                                               (type call)
                                               stream
                                               view
                                               &key)
  (write-string "<call>" stream))

(clim:define-presentation-type call-tree ())

(clim:define-presentation-method clim:present ((object node)
                                               (type call-tree)
                                               stream
                                               view
                                               &key)
  (write-string "<call tree>" stream))

;;; Flamegraph view

(defclass flamegraph-view (clim:view)
  ((scale :initarg :scale
          :reader  scale)))

(clim:define-presentation-method clim:present ((object node)
                                               (type   call)
                                               stream
                                               (view   flamegraph-view)
                                               &key)
  (let ((scale (* 400 (/ total-count) (expt 2 (scale view)))))
    (with-accessors ((count node-count) (call node-call)) object
      (multiple-value-bind (x y) (clim:stream-cursor-position stream)
        (let* ((width      (* scale count))
               (style      (clim:make-text-style :fix nil :small))
               (name       (when call
                             (sb-sprof::node-name call)))
               (text       (if call
                               (princ-to-string name)
                               "<root>"))
               (text-width (clim:text-size stream text :text-style style)) ; TODO don't do this twice
               (height     (nth-value 1 (clim:text-size stream text)))
               (region     (clim:make-rectangle* x y (+ x width) (+ y height)))
               (old-clip   (clim:medium-clipping-region stream)))
          (unwind-protect
               (progn
                 (setf (clim:medium-clipping-region stream) region)
                 (clim:draw-design stream region :ink (name+depth+position->color
                                                       name depth position))
                 (when (> width text-width) ; FIXME We wouldn't need this if the clipped text wouldn't enlarge the output record's bounding rectangle
                   (clim:draw-text* stream text (+ x (/ width 2)) (+ y (/ height 2))
                                    :align-x :center :align-y :center
                                    :text-style style)))
            (setf (clim:medium-clipping-region stream) old-clip)))
        (setf (clim:stream-cursor-position stream)
              (values x (nth-value 1 (clim:stream-cursor-position stream))))

        ;; FIXME This doesn't work. why?
        #+no (surrounding-output-with-border (stream :padding 1
                                                     :filled  t
                                                     :shape   :rounded
                                                     :ink     (name+depth+position->color
                                                               name depth position))
               (setf (medium-clipping-region stream)
                     (make-rectangle* x y (+ x (* 2 count)) (+ y 20)))

               (princ #\[ stream)
               (setf (stream-cursor-position stream)
                     (values (+ x (* 2 count)) (nth-value 1 (stream-cursor-position stream))))
               (princ #\] stream)
               (setf (medium-clipping-region stream) +everywhere+))
        #+no (setf (stream-cursor-position stream)
                   (values x (nth-value 1 (stream-cursor-position stream))))))))

(clim:define-presentation-method clim:present ((object node)
                                               (type   call-tree)
                                               stream
                                               (view   flamegraph-view)
                                               &key)
  (let ((total-count (node-count object))) ; root node
    (labels ((present-node (node &optional (depth 0) (position 0))
               (let* ((presentation (clim:present node `(call ,depth ,position ,total-count)
                                                  :stream stream :view view :single-box t))
                      (y            (nth-value 3 (clim:bounding-rectangle* presentation)))
                      (position     0))
                 (setf (clim:stream-cursor-position stream)
                       (values (clim:stream-cursor-position stream) y))
                 (map nil (lambda (child)
                            (let ((child-presentation
                                    (present-node child (1+ depth) (incf position))))
                              (multiple-value-bind (x1 y1 x2 y2)
                                  (clim:bounding-rectangle* child-presentation)
                                (declare (ignore x1 y1 y2))
                                (setf (clim:stream-cursor-position stream)
                                      (values x2 y)))))
                      (hash-table-values (node-children node)))
                 presentation)))   ; TODO interface without hash-table
      (present-node object))))

(defun display-flame-graph (frame pane)
  (when-let ((traces (selected-traces frame)))
    (let ((scale (clim:gadget-value (clim:find-pane-named frame 'scale))))
      (clim:present (traces->tree traces) 'call-tree
                    :stream pane
                    :view   (make-instance 'flamegraph-view :scale scale)))
    (clim:change-space-requirements pane :resize-frame nil)))
