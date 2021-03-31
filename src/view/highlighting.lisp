;;;; highlighting.lisp --- Utilities for output record highlighting.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view)

(labels ((draw-it (function stream)
           (clim:surrounding-output-with-border (stream :shape :drop-shadow
                                                        :background clim:+background-ink+)
             (clim:with-end-of-line-action (stream :allow)
               (setf (clim:stream-cursor-position stream) (values 0 0))
               (clime:with-temporary-margins (stream :left `(:absolute 0))
                 (funcall function stream)))))
         (adjust-position (original-record annotation-record stream)
           (clim:with-bounding-rectangle* (sx1 sy1 sx2 sy2)
               (clim:untransform-region (clim:sheet-native-transformation stream)
                                        (clim:sheet-native-region stream))
             (clim:with-bounding-rectangle* (ox1 oy1 ox2 oy2) original-record
               (clim:with-bounding-rectangle* (ax1 ay1 ax2 ay2) annotation-record
                 (let* ((owidth   (- ox2 ox1))
                        (awidth   (- ax2 ax1))
                        (x        (max sx1 (- ox1 (if (> (+ ox1 ax2) sx2)
                                                      (- (+ ox1 ax2) sx2)
                                                      (/ (- awidth owidth) 2)))))
                        (y        (+ oy2 8)))
                   (setf (clim:output-record-position annotation-record)
                         (values x y))))))))

  (defun call-as-highlighting (function type record stream state)
    (let ((annotation-record (clim:with-output-to-output-record (stream)
                               (draw-it (lambda (stream)
                                          (funcall function type record stream state))
                                        stream))))
      (adjust-position record annotation-record stream)
      (ecase state
        (:highlight   (clim:replay annotation-record stream))
        (:unhighlight (clim:repaint-sheet stream annotation-record))))))
