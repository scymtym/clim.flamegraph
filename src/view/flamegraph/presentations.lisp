;;;; presentations.lisp --- Presentation types used in the view.flamegraph module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.flamegraph)

(clim:define-presentation-type node
    (&optional (depth 0) (position 0) (scale 1))
  :inherit-from '(t))

(clim:define-presentation-type tree (&optional (shrink 1))
  :inherit-from '(t))

;;; Default view

(clim:define-presentation-method clim:present ((object model::standard-node)
                                               (type   node)
                                               (stream t)
                                               (view   t)
                                               &key)
  (clim:present (model:name object) `(view:name :qualified? t)
                :stream stream :view view)
  (format stream " ~:D hit~:P" (model:hit-count object)))

(clim:define-presentation-method clim:present ((object model::standard-node)
                                               (type   tree)
                                               (stream t)
                                               (view   t)
                                               &key)
  (write-string "<call tree>" stream))

;;; Flamegraph view

(defclass flamegraph-view (clim:view) ; TODO store this somewhere else
  ((%total-hit-count :initarg  :total-hit-count
                     :reader   total-hit-count)
   ;; Cache for graphical properties
   (%text-style      :initarg  :text-style
                     :reader   text-style
                     :initform (clim:make-text-style :fix nil :smaller))
   (%char-size       :accessor %char-size
                     :initform nil)))

(defun name+depth+position->color (name depth position)
  (declare (ignore depth position))
  (view:symbol-color name))

(defun char-size (view stream)
  (values-list
   (or (%char-size view)
       (setf (%char-size view)
             (let ((style (text-style view)))
               (list (clim:text-size stream "W" :text-style style)
                     (nth-value 1 (clim:text-size stream "J" :text-style style))))))))

(clim:define-presentation-method clim:present ((object model::standard-node)
                                               (type   node)
                                               (stream clim:extended-output-stream)
                                               (view   flamegraph-view)
                                               &key)
  (with-accessors ((function model:name) (hit-count model:hit-count)) object
    (multiple-value-bind (char-width char-height) (char-size view stream)
      (multiple-value-bind (x y) (clim:stream-cursor-position stream)
        (let* ((name       (if (stringp function) ; TODO make a proper abstraction for this stuff
                               function
                               (model:name function)))
               ;; Text
               (text       (model:name-string function))
               ;; Dimensions
               (width      (* scale hit-count))
               (text       (cond ((< width (* 3 char-width))
                                  nil)
                                 ((< width (* (length text) char-width))
                                  (let ((end (1- (truncate width char-width))))
                                    (concatenate 'string (subseq text 0 end) "…")))
                                 (t
                                  text)))
               (background (name+depth+position->color name depth position))) ; TODO distinguish function and name properly
          (clim:draw-rectangle* stream x y (+ x width) (+ y char-height) :ink background)
          (unless (emptyp text)         ; TODO should this happen?
            (clim:draw-text* stream text (+ x (/ width 2)) (+ y (/ char-height 2))
                             :align-x :center :align-y :center
                             :text-style (text-style view))))

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

(defvar *highlight-text-style*
  (clim:make-text-style :fix nil :smaller))

(defun present-function-info (function stream)
  (let ((*print-right-margin* nil) ; TODO correct?
        (*print-miser-width*  nil))
    (clim:present function `(view:name :qualified? t :style :dark)
                  :stream stream)
    (terpri stream)
    (let ((hit-count          0 #+no (model:hit-count node))
          (children-hit-count 0 #+no (reduce #'+ (hash-table-values (model::%children node)) ; TODO
                                      :key #'model:hit-count))
          (total-hit-count    (total-hit-count (clim:stream-default-view stream))))
      (clim:present hit-count `(view:hit-count :total-hit-count ,total-hit-count)
                    :stream stream)
      (format stream " self ")
      (clim:present (- hit-count children-hit-count) `(view:hit-count :total-hit-count ,total-hit-count)
                    :stream stream)

      ;; (format stream "~%~:D hit~:P" )

      (unless (stringp function)
        (format stream "~2%")
        (clim:present function `(view:call-statistics :total-hit-count ,1000000)
                      :stream stream)))))

(labels ((draw-info-text (stream node)
           (let ((function             (model:name node))
                 (*print-right-margin* nil)
                 (*print-miser-width*  nil))
             (clim:present function `(view:name :qualified? t :style :dark)
                           :stream stream)
             (terpri stream)
             (let ((hit-count          (model:hit-count node))
                   (children-hit-count (reduce #'+ (hash-table-values (model::%children node)) ; TODO
                                               :key #'model:hit-count))
                   (total-hit-count    (total-hit-count (clim:stream-default-view stream))))
               (clim:present hit-count `(view:hit-count :total-hit-count ,total-hit-count)
                             :stream stream)
               (format stream " self ")
               (clim:present (- hit-count children-hit-count) `(view:hit-count :total-hit-count ,total-hit-count)
                             :stream stream)

               ;; (format stream "~%~:D hit~:P" )

               (unless (stringp function)
                 (format stream "~2%")
                 (clim:present function `(view:call-statistics :total-hit-count ,1000000)
                               :stream stream)))))
         (draw-info (stream #+no record node)
           (clim:with-drawing-options (stream :text-style *highlight-text-style*)
             (draw-info-text stream node)
             (when-let ((children (hash-table-values (model::%children node))))
               (loop :for i              :from 0
                     :for child          :in (sort children #'> :key #'model:hit-count)
                     :for hit-ratio      = (/ (model:hit-count child)
                                              (model:hit-count node))
                     :for previous-angle = 0 :then angle
                     :for angle          = (+ previous-angle (* 2 pi hit-ratio))
                     :for ink   = (clim:make-contrasting-inks 8 (mod i 8))
                     :do (clim:draw-circle* stream (- 0 48) (+ 0 40) 40
                                            :filled      t
                                            :ink         ink
                                            :start-angle previous-angle
                                            :end-angle   angle))))
           #+old (clim:with-bounding-rectangle* (x1 y1 x2 y2) record
             (let* ((xc    (/ (+ x1 x2) 2))
                    (yc    (/ (+ y1 y2) 2))
                    (style *highlight-text-style*))
               (clim:surrounding-output-with-border (stream :shape      :drop-shadow
                                                            :background clim:+background-ink+)
                 ;; Include the RECORD so the border will be at least
                 ;; as big.
                 (clim:draw-design stream record :ink clim:+transparent-ink+)

                 (clime:with-temporary-margins (stream :left `(:absolute ,xc))
                   (clim:with-end-of-line-action (stream :allow)
                     (clim:with-drawing-options (stream :text-style style)
                       (setf (clim:stream-cursor-position stream) (values xc y1))
                       (draw-info-text stream node)
                       #+no (clim:draw-text* stream text xc y1
                                             :align-x :left :align-y :top :text-style style)
                       ;;
                       (when-let ((children (hash-table-values (model::%children node))))
                         (loop :for i              :from 0
                               :for child          :in (sort children #'> :key #'model:hit-count)
                               :for hit-ratio      = (/ (model:hit-count child)
                                                        (model:hit-count node))
                               :for previous-angle = 0 :then angle
                               :for angle          = (+ previous-angle (* 2 pi hit-ratio))
                               :for ink   = (clim:make-contrasting-inks 8 (mod i 8))
                               :do (clim:draw-circle* stream (- xc 48) (+ y1 40) 40
                                                      :filled      t
                                                      :ink         ink
                                                      :start-angle previous-angle
                                                      :end-angle   angle))))))))))
         (adjust-position (record stream)
           (clim:with-bounding-rectangle* (ox1 oy1 ox2 oy2) record
             (clim:with-bounding-rectangle* (sx1 sy1 sx2 sy2) stream
               (when (> ox2 sx2)
                 (setf (clim:output-record-position record)
                       (values (max 0 (- ox1 (- ox2 sx2))) oy1)))))))

  (clim:define-presentation-method clim:highlight-presentation
      ((type   node)
       (record t)
       (stream clim:extended-output-stream)
       (state  t))
    (view::call-as-highlighting
     (lambda (type record stream state)
       (declare (ignore type state))
       (draw-info stream (clim:presentation-object record)))
     type record stream state)))

(clim:define-presentation-method clim:present ((object model::standard-tree)
                                               (type   tree)
                                               (stream clim:extended-output-stream)
                                               (view   flamegraph-view)
                                               &key)
  (let* ((root        (model::root object))
         (depth-limit (or 400 #+no (depth-limit view) most-positive-fixnum))
         (total-count (model:hit-count root))
         (sheet-width (clim:bounding-rectangle-width (clim:sheet-region stream)))
         (scale       (* sheet-width shrink (/ total-count) (expt 2 0 #+no (scale view)))))
    (labels ((present-node (node &optional (x (clim:stream-cursor-position stream)) (depth 0) (index 0))
               (let* ((presentation (clim:present node `(node ,depth ,index ,scale)
                                                  :stream stream :view view :single-box t))
                      (y            (clim:bounding-rectangle-max-y presentation))
                      (index        index))
                 (setf (clim:stream-cursor-position stream) (values x y))
                 (when (< depth depth-limit)
                   (map nil (lambda (child)
                              (let ((width (* scale (model:hit-count child))))
                                (progn ; when (> width 2)
                                  (setf (clim:stream-cursor-position stream)
                                        (values x y))
                                  (present-node child x (1+ depth) index))
                                (incf index)
                                (incf x width)))
                        (sort (hash-table-values (model::%children node)) #'>
                              :key #'model:hit-count))) ; TODO interface without hash-table
                 presentation)))
      (present-node root))))
