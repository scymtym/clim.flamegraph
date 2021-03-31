(cl:in-package #:clim.flamegraph.view.timeline)

(defclass timeline2-pane (clim:hbox-pane)
  ((%left  :initarg  :left
           :accessor left)
   (%right :initarg  :right
           :accessor right))
  (:default-initargs
   :left  (error "must supply :left initarg")
   :right (error "must supply :right initarg")))

(defmethod initialize-instance :after ((instance timeline2-pane) &key left right)
  (clim:sheet-adopt-child instance left)
  (clim:sheet-adopt-child instance right))

; (defmethod clim:allocate-space)

(defmethod clim:move-sheet ((sheet timeline2-pane) dx dy)
                                        ; (setf (dx sheet) dx (dy sheet) dy)
  (clim:with-bounding-rectangle* (x1 y1 x2 y2) (clim:sheet-region sheet)
    (clim:move-sheet (left sheet) 0 dy)
    (setf (clim:sheet-region (left sheet))
          (clim:make-rectangle* x1 y1 (+ x1 100) y2)) ; TODO this causes additional repaints

    (clim:move-sheet (right sheet) (+ dx 100) dy)
    (setf (clim:sheet-region (right sheet))
          (clim:make-rectangle* (+ x1 -100 (- 0 #+no dx)) y1 (+ x2 -100 (- 0 #+no dx)) y2)
          #+no (clim:make-rectangle* (- dx) 0 (+ 500 (- dx)) 500)))
  #+no (when (clim:sheet-viewable-p sheet)
         (climi::dispatch-repaint sheet (clim:sheet-region sheet))))
