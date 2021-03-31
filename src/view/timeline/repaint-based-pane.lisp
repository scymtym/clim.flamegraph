(cl:in-package #:clim.flamegraph.view.timeline)

(defclass timeline-pane (clim:application-pane)
  ((%dx :accessor dx)
   (%dy :accessor dy))
  (:default-initargs
   :display-time nil))

(defmethod clim:compose-space ((pane timeline-pane) &key width height)
  (clim:make-space-requirement :width 3000 :height 3000))

(defmethod clim:move-sheet ((sheet timeline-pane) dx dy)
  (setf (dx sheet) dx (dy sheet) dy)
  (when (clim:sheet-viewable-p sheet)
    (climi::dispatch-repaint sheet (clim:sheet-region sheet))))

(defmethod clim:redisplay-frame-pane ((frame clim:application-frame)
                                      (pane  timeline-pane)
                                      &key force-p)
  (declare (ignore force-p))
                                        ; (clim:draw-circle* pane 50 50 40)
  )

(defmethod clim:handle-repaint ((pane timeline-pane) (event t))
  (clim:with-drawing-options (pane :clipping-region (clim:make-rectangle* 0 0 100 100))
    (clim:with-translation (pane 0 (dy pane))
      (clim:draw-text* pane (format nil "~D ~D" (dx pane) (dy pane)) 50 50 :align-x :center :align-y :center)))

  (clim:with-drawing-options (pane :clipping-region (clim:make-rectangle* 100 0 500 500))
    (clim:with-drawing-options (pane :transformation  (clim:make-translation-transformation
                                                       (+ 100 (dx pane)) (dy pane)))
      (clim:draw-design pane (clim:sheet-region pane) :ink clim:+green+)
      (loop :for i :from 0 :to 1000 :by 100
            :do (clim:draw-circle* pane i 50 40 :ink clim:+red+)
                (clim:draw-text* pane (format nil "~D" i) i 50 :align-x :center :align-y :center)))))

#+no (defmethod clim:handle-repaint ((pane timeline-pane) (event t))

       (let ((climi::*inhibit-dispatch-repaint* t))
         (let ((old        (clim:sheet-transformation pane))
               (old-region (clim:sheet-region pane)))
           (setf (clim:sheet-transformation pane) clim:+identity-transformation+
                                        ; (clim:sheet-region pane)         (clim:make-rectangle* 0 0 100 100)
                 )

           (unwind-protect
                (clim:with-drawing-options (pane :clipping-region (clim:make-rectangle* 0 0 100 100))
                  (clim:draw-design pane (clim:sheet-region pane) :ink clim:+blue+)
                  (clim:draw-text* pane "foo" 20 20))
             (setf (clim:sheet-transformation pane) old
                   (clim:sheet-region pane)         old-region
                   )))

         (let ((old (clim:sheet-region pane)))
           (setf (clim:sheet-region pane) (clim:region-difference
                                           old
                                           (clim:untransform-region
                                            (clim:sheet-transformation pane)
                                            (clim:make-rectangle* 0 0 100 100))))
           (unwind-protect
                (progn ; clim:with-drawing-options (pane :clipping-region )
                  (loop :for i :from 100 :to 1000 :by 100
                        :do (clim:draw-circle* pane i 50 40 :ink clim:+red+)))
             (setf (clim:sheet-region pane) old)))))
