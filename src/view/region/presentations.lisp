;;;; presentations.lisp --- .
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.region)

(clim:define-presentation-type inspectable ()
  :inherit-from '(t))

;;; `call'

(clim:define-presentation-type call (&key (name-qualified? nil) (arguments? nil))
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   call)
                                               (stream t)
                                               (view   t)
                                               &key)
  (clim:with-drawing-options (stream :text-family :fix)
    (write-string "(" stream)

    (clim:present object `(function1 :name-qualified? ,name-qualified?)
                  :stream stream :view view)

    (when (and arguments? (compute-applicable-methods #'model::values* (list object)))
      (when-let ((values (model::values* object)))
        (write-string " " stream)
        (clim:format-textual-list
         values (lambda (value stream)
                  (clim:present value 'argument :stream stream :view view))
         :stream stream :separator " ")))

    (when-let ((lock (when (compute-applicable-methods #'model::object (list object))
                       (model::object object))))
      (clim:with-output-as-presentation (stream lock 'argument)
        (write-string (view::truncate-string (princ-to-string lock) 30) stream)))

    (write-string ")" stream)
    ))

;;; `function1'

(clim:define-presentation-type function1 (&key (name-qualified? nil))
  :inherit-from 'inspectable)

(clim:define-presentation-method clim:present ((object t)
                                               (type   function1)
                                               (stream t)
                                               (view   t)
                                               &key)
  (clim:present (model:name object) `(view::name :qualified? ,name-qualified?) ; TODO maybe don't cons here?
                :stream stream :view view))

;;; `argument'

(clim:define-presentation-type argument ()
  :inherit-from 'inspectable)

(clim:define-presentation-method clim:present ((object t)
                                               (type   argument)
                                               (stream t)
                                               (view   t)
                                               &key)
  (let ((string (handler-case
                    (princ-to-string object)
                  (error (condition)
                    "«error printing argument»"))))
    (clim:with-drawing-options (stream :ink clim:+dark-goldenrod+)
      (write-string (view::single-line (view::truncate-string string 40)) stream))))

;;; `region'

(clim:define-presentation-type region (&key (start-time 0) (end-time 0) (scale 1) (height 16)) ; TODO start-time, end-time, scale defaults are a HACK
  :inherit-from 'inspectable)

(clim:define-presentation-method clim:present ((object model::standard-region) ; TODO remove
                                               (type   region)
                                               (stream t)
                                               (view   t)
                                               &key)
  (princ object stream))

(defun draw-region-text (region stream view &key (name?       t)
                                                 (arguments?  t)
                                                 (duration?   t)
                                                 (statistics? nil))
  (clim:with-drawing-options (stream :text-style (clim:make-text-style nil :italic :smaller))
    (when name?
      (clim:present region `(call :name-qualified? ,(eq name? :qualified)
                                  :arguments?      ,arguments?)
                    :stream stream :view view)
      (write-string " " stream))
    (when duration?
      (clim:with-drawing-options (stream :text-style (clim:make-text-style nil :italic :smaller))
        (time:print-human-readable-duration stream (model:duration region))))
    (when statistics?
      (fresh-line stream)
      (clim:stream-increment-cursor-position stream 0 8)
      (clim:present (model:name region) 'view::call-statistics
                    :stream stream :view view))))

(defun resource-color (resource)
  (view::hash-color (sxhash (princ-to-string resource)) :desaturize? t))

(clim:define-presentation-method clim:present ((object model::standard-region)
                                               (type   region)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (let* ((start (- (model:start-time object) start-time))
         (end   (max (- (or (model:end-time object) end-time)
                        start-time)
                     (+ start (/ 3 scale))))
         (width (- end start))
         (cx    (/ (+ start end) 2))
         (cy    (/ height 2))

         (transformation (clim:medium-transformation stream)))

    (cond ((<= (clim:transform-distance transformation width 0) 2)
           (clim:draw-line* stream start 0 start height))

          (t
           (typecase object
             (model::wait-region
              (let ((object (model::object object)))
                (unless (numberp object)
                  (clim:draw-rectangle* stream start 0 end height
                                        :ink         (clim:make-rectangular-tile
                                                      (clim:make-pattern
                                                       #2A((0 0 0 1 1 1)
                                                           (0 0 1 1 1 0)
                                                           (0 1 1 1 0 0)
                                                           (1 1 1 0 0 0)
                                                           (1 1 0 0 0 1)
                                                           (1 0 0 0 1 1))
                                                       (list clim:+white+ ; clim:+transparent-ink+
                                                             (resource-color object)))
                                                      6 6)
                                        :filled      t)))
              #+no (multiple-value-bind (dx dy) (clim:untransform-distance transformation 4 4)
                     (clim:draw-rectangle* stream (+ start dx) (+ 0 dy) (- end dx) (- height dy)
                                           :ink         clim:+white+
                                           :filled      t))
              (clim:draw-rectangle* stream start 0 end height
                                    :ink         clim:+red+
                                    :filled      nil))
             (model::block-region
              (clim:draw-rectangle* stream start 0 end height
                                    :ink         (resource-color (model::object object))
                                    :filled      t)
              (clim:draw-rectangle* stream start 0 end height
                                    :ink         clim:+black+
                                    :filled      nil))
             (t
              (clim:draw-rectangle* stream start 0 end height
                                    :filled      nil
                                    :line-dashes (if (model:finished? object)
                                                     nil
                                                     '(4 4)))))
                                        ; (clim:draw-ellipse* stream cx 4  0 8 (/ width 2) 0)
           (setf (clim:stream-cursor-position stream)
                 (clim:transform-position transformation start 4))
           (clim:stream-increment-cursor-position stream 2 0)
           (clim:with-drawing-options (stream :clipping-region (clim:make-rectangle* start 0 end height))
             (clim:with-end-of-line-action (stream :allow)
               (let ((*print-right-margin* most-positive-fixnum)
                     (*print-miser-width*  nil))
                 (draw-region-text object stream view
                                   :name?      (>= (* scale width) 200)
                                   :arguments? (> (clim:transform-distance transformation width 0) 100) ; TODO hack
                                   :duration?  (>= (* scale width) 50)))))

           #+no (let* ((record (clim:with-output-to-output-record (stream)
                                 (when (typep object 'model::wait-region)
                                   (clim:draw-rectangle* stream 0 0 width 20
                                                         :ink         clim:+salmon+
                                                         :filled      t))
                                 (clim:draw-rectangle* stream 0 0 width 20
                                                       :filled      nil
                                                       :line-dashes (if (model:finished? object)
                                                                        nil
                                                                        '(4 4)))
                                 (clim:draw-circle* stream cx 4 8)
                                 (setf (clim:stream-cursor-position stream)
                                       (clim:transform-position
                                        (clim:medium-transformation stream) cx 4))
                                 (clim:with-end-of-line-action (stream :allow)
                                   (when (>= (* scale width) 500)
                                     (clim:present object 'call :stream stream :view view)
                                     (write-string " " stream))
                                   (when (>= (* scale width) 50)
                                     (clim:with-drawing-options (stream :text-style (clim:make-text-style nil :italic :smaller))
                                       (time:print-human-readable-duration stream (model:duration object)))))))
                       (x1     (- cx (/ (clim:bounding-rectangle-width record) 2)))
                       (y1     (- cy (/ (clim:bounding-rectangle-height record) 2))))
                  (setf (clim:output-record-position record)
                        (clim:transform-position (clim:medium-transformation stream) start 0 ; x1 y1
                                                 )
                        )
                  (clim:add-output-record record (clim:stream-output-history stream))
                  (clim:replay record stream))))))

(defclass standard-region-output-record (clim:standard-presentation)
  ())

(labels ((draw-info (record region stream)
           (let* ((view  (clim:stream-default-view stream))
                  (style (clim:make-text-style nil nil nil))) ; TODO
             (clim:with-bounding-rectangle* (x1 y1 x2 y2) record
               (clim:surrounding-output-with-border (stream :shape      :drop-shadow
                                                            :background clim:+background-ink+)
                 (clim:with-end-of-line-action (stream :allow) ; TODO handle this systematically
                   ;; Include the RECORD so the border will be at least
                   ;; as big.
                   (let* ((y-offset (+ (- y2 y1) 8))
                          (bounds (clim:untransform-region (clim:sheet-native-transformation stream)
                                                           (clim:sheet-native-region stream)))
                          (bounds (clim:transform-region
                                   (clim:make-scaling-transformation ; TODO bad. better adjust {min,max}-x by fixed amount
                                    .95 1
                                    (clim:make-point
                                     (/ (+ (clim:bounding-rectangle-min-x bounds)
                                           (clim:bounding-rectangle-max-x bounds))
                                        2)
                                     (/ (+ (clim:bounding-rectangle-min-y bounds)
                                           (clim:bounding-rectangle-max-y bounds))
                                        2)))
                                   bounds))
                          (bounds* (clim:region-intersection bounds (clim:transform-region
                                                                     (clim:make-translation-transformation 0 y-offset)
                                                                     record))))
                     (clim:draw-design stream bounds :ink clim:+red+)
                     (clim:draw-design stream bounds :ink clim:+transparent-ink+)
                     (setf (clim:stream-cursor-position stream)
                           (values (clim:bounding-rectangle-min-x bounds*)
                                   (clim:bounding-rectangle-min-y bounds*))
                                        ; (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
                           )
                     (clime:with-temporary-margins (stream :left `(:absolute ,(clim:bounding-rectangle-min-x bounds)))
                       (draw-region-text region stream view
                                         :name? :qualified :statistics? t)))))))))

  (clim:define-presentation-method clim:highlight-presentation
      ((type   region)
       (record t)
       (stream clim:extended-output-stream)
       (state  t))
    (view::call-as-highlighting
     (lambda (type record stream state)
       (declare (ignore type state))
       (let ((region (clim:presentation-object record))
             (view   (clim:stream-default-view stream)))
         (draw-region-text region stream view :name? :qualified :statistics? t)))
     type record stream state)
    #+no (clim:with-output-recording-options (stream :draw t :record nil)
           (draw-info record (clim:presentation-object record) stream)))

  (clim:define-presentation-method clim:highlight-presentation
      ((type   region)
       (record t)
       (stream clim:extended-output-stream)
       (state  (eql :unhighlight)))
    (call-next-method)
    #+no (let ((record (clim:with-output-to-output-record (stream)
                         (draw-info record (clim:presentation-object record) stream))))
           (clim:repaint-sheet stream record))))

;;; Regions

(clim:define-presentation-type regions (&key start-time
                                             end-time
                                             (scale       200)
                                             (depth-limit 100)
                                             (line-height 16))
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   regions)
                                               (stream t)
                                               (view   t)
                                               &key)
  (let+ ((type `(region :start-time ,start-time ; (model:start-time (first-elt object))
                        :end-time   ,end-time
                        :scale      ,scale
                        :height     ,line-height))
         ((&labels displayable? (region depth)
            (and (< depth depth-limit)
                 (model:finished? region) ; TODO show unfinished
                 (>= (* scale (model:duration region)) 2))))
         ((&labels rec (region depth)
            (clim:present region type :stream stream :view view :record-type 'standard-region-output-record)
            (let ((children (model:children region)))
              (cond ((not children))
                    ((notany (rcurry #'displayable? (1+ depth)) children)
                     (clim:stream-increment-cursor-position stream 0 line-height)
                     (clim:with-translation (stream 0 0 ; line-height
                                                    )
                       (write-string "…" stream)))
                    (t
                     (clim:with-translation (stream 0 line-height)
                       (map nil (rcurry #'rec (1+ depth)) children))))))))
    (map nil (rcurry #'rec 0) (remove-if-not (rcurry #'displayable? 0) object)))) ; TODO how should we handle omitted top-level regions?
