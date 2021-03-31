(defun foo (frame pane ink)
  (clim:draw-design pane (clim:sheet-region pane) :filled t :ink ink)
  (loop :for i :from 0 :to 1000 :by 100
        :do (clim:draw-circle* pane i 50 40 :ink ink)
            (clim:draw-circle* pane i 50 40 :filled nil :ink clim:+black+)
            (clim:with-output-as-presentation (pane i 'integer)
              (clim:draw-text* pane (format nil "~D" i) i 50 :align-x :center :align-y :center))))
