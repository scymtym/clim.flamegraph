;;;; timeline-pane.lisp --- .
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.timeline)

(defclass lane-description-pane (clim:application-pane)
  ((%model       :initarg  :model
                 :reader   model)
   (%depth-limit :initarg  :depth-limit
                 :accessor depth-limit
                 :initform 10))
  (:default-initargs
   :end-of-line-action :allow
   :end-of-page-action :allow))

(defun lane-height (lane &key (depth-limit most-positive-fixnum)) ; TODO
  (let* ((line-height     20)
         (lane-separation 8)
         (height          (max 40 (* line-height (1+ (min depth-limit (model::depth lane)))))))
    (values height (+ lane-separation height))))

(defmethod clim:redisplay-frame-pane ((frame clim:application-frame)
                                      (pane  lane-description-pane)
                                      &key force-p)
  (declare (ignore force-p))
  (clim:with-drawing-options (pane :text-size :smaller)
    (let ((line-height     20)
          (lane-separation 20))
      (clim:with-translation (pane 0 28)
        (loop :for i :from 0
              :for y = 0 :then (+ y next-lane)
              :for lane :in (lanes (model pane))
              :for (lane-height next-lane) = (multiple-value-list (lane-height lane :depth-limit (depth-limit pane))) ; TODO
              :unless (zerop i)
              :do (let ((separator-y (+ y (- next-lane) (alexandria:lerp 0.5 lane-height next-lane))))
                    (clim:draw-line* pane 0 separator-y 100 separator-y
                                     :line-dashes '(4 4)))
              :do (multiple-value-bind (x* y*)
                      (clim:transform-position
                       (clim:medium-transformation pane) 0 y)
                    (setf (clime:stream-text-margins pane) `(:left (:absolute ,x*)
                                                             :top  (:absolute ,y*))
                          (clim:stream-cursor-position pane) (values x* y*)))
                  (clim:with-output-as-presentation (pane lane 'thread)
                    (format pane (if (string= (description lane) "GLOBAL")
                                     "Context"
                                     "Thread")))
                  (terpri pane)
                  (clim:with-text-size (pane :smaller)
                    (princ (clim.flamegraph.view::truncate-string (description lane) 20) pane)))))))

(defclass timeline-pane (clim:application-pane)
  ((%model       :initarg  :model
                 :reader   model)
   (%scale       :initarg  :scale
                 :accessor scale
                 :initform (expt 10 1.5))
   (%depth-limit :initarg  :depth-limit
                 :accessor depth-limit
                 :initform 3))
  (:default-initargs
   :end-of-line-action :allow
   :end-of-page-action :allow))

(defvar *start-time*)
(defvar *end-time*)

(defmethod display-model ((pane timeline-pane) (model timeline))
  (let ( ; (pane-width (clim:bounding-rectangle-width (clim:sheet-region pane)))
        (scale       (scale pane))
        (start-time  (model:start-time model))
        (end-time    (model:end-time   model))
        (y           0))
    (clim:with-scaling (pane scale 1)
      (let ((tick-text-height (nth-value 1 (clim:text-size pane "1")))
            (tick-height      2))
        (clim:with-translation (pane 0 tick-text-height)
          (loop :for i :from 0
                :for time :from (- start-time start-time) :to (+ (- end-time start-time) #+no (* 100 (/ scale))) :by (* 100 (/ scale)) ; One tick every 100 pixels
                :for label = (with-output-to-string (stream)
                               (text.orders-of-magnitude:print-human-readable-duration
                                stream time ; (+ start-time time)
                                ))
                ; :do (format *trace-output* "~A ~A~%" time label) (force-output *trace-output*)
                :do (clim:draw-line* pane time (- tick-height) time tick-height)
                    (clim:draw-text* pane label time 0
                                     :align-x (if (zerop i) :left :center)
                                     :align-y :bottom))
          (clim:draw-line* pane (- start-time start-time) 0 (- end-time start-time) 0))
        (incf y 28 ; (+ tick-text-height tick-height 8)
              ))

      (let ((*start-time* start-time)
            (*end-time*   end-time))
        (clim:with-translation (pane 0 y)
          (loop :for i :from 0
                :for offset = 0 :then (+ offset next-lane)
                :for lane :in (lanes model)
                :for (lane-height next-lane) = (multiple-value-list (lane-height lane :depth-limit (depth-limit pane)))
                :do (unless (zerop i)
                      (let ((separator-y (+ offset (- next-lane) (alexandria:lerp .5 lane-height next-lane))))
                        (clim:draw-line* pane
                                         (- start-time start-time) separator-y
                                         (- end-time start-time) separator-y
                                         :line-dashes '(4 4))))
                    (clim:with-translation (pane 0 offset)
                      (display-model pane lane))))))))

(defun total-used (event)
  (reduce #'+ (getf (model::properties event) :used)))

(defun used (event generation)
  (if-let ((used (getf (model::properties event) :used)))
    (reduce #'+ (reverse used) :end (1+ generation))
    0))

(defmethod display-model ((pane timeline-pane) (model lane))
  (let ((traces (remove-if-not (alexandria:of-type 'clim.flamegraph.model::standard-trace)
                               (elements model))))
    (clim:present traces 'traces :stream pane))

  (let ((height 80))
   (clim:with-translation (pane 0 height)
     (clim:with-scaling (pane 1 -1)
       (let* ((events   (remove-if-not (alexandria:of-type 'clim.flamegraph.model::standard-event)
                                       (elements model)))
              (space-count (if (alexandria:emptyp events)
                               0
                               (length (getf (model::properties (elt events 0)) :used))))
              (used-max (reduce #'max events
                                :key #'total-used :initial-value 1)))
         (loop :for j :from (1- space-count) :downto 0
               :for ink = (clim:make-contrasting-inks (min space-count 8) (mod (mod j space-count) 8))
               :do (loop :for i :from 0
                         :for previous = nil :then current
                         :for current :in events
                         :for previous-time  = time
                         :for previous-value0 = value0
                         :for previous-value1 = value1
                         :for time           = (- (model:time current) *start-time*)
                         :for value0         = (* height (/ (used current (1- j)) used-max))
                         :for value1         = (* height (/ (used current j) used-max))
                         :when previous
                         :do (clim:draw-polygon* pane (list previous-time previous-value1 time          value1
                                                            time          value0          previous-time previous-value0)
                                                 :filled t :ink ink)
                             (clim:draw-line* pane previous-time previous-value1 time value1)
                         :do (multiple-value-bind (dx dy) (clim:untransform-distance
                                                           (clim:medium-transformation pane) 3 3)
                               (clim:with-output-as-presentation (pane current 'event)
                                 (clim:draw-ellipse* pane time value1 dx 0 0 dy :ink (case (model:name current)
                                                                                       (:gc-start clim:+green+)
                                                                                       (:gc-end   clim:+red+)
                                                                                       (t         clim:+black+)))))))))))
  #+no (map nil (alexandria:curry #'display-model pane) (elements model))

  (clim:with-translation (pane 0 20)
    (let ((regions (remove-if-not (alexandria:of-type 'clim.flamegraph.model::standard-region)
                                  (elements model))))
      (clim:present regions `(clim.flamegraph.view.region::regions
                              :start-time  ,*start-time*
                              :end-time    ,*end-time*
                              :scale       ,(scale pane)
                              :depth-limit ,(depth-limit pane))
                    :stream pane))))

(defmethod display-model ((pane timeline-pane) (model model::standard-region))
  #+no (clim:present model `(clim.flamegraph.view.region::regions :scale ,(scale pane))
                :stream pane)
  ; (clim:draw-text* pane (princ-to-string model) (- (model:start-time model) *start-time*) 0)
  )

(defmethod clim:redisplay-frame-pane ((frame clim:application-frame)
                                      (pane  timeline-pane)
                                      &key force-p)
  (declare (ignore force-p))
  (clim:with-translation (pane 8 8)     ; TODO use padding, border etc
    (clim:with-drawing-options (pane :text-size :smaller)
      #-mezzano (display-model pane (model pane)))))

(defmethod clim:replay-output-record :around ((record clim.flamegraph.view.region::standard-region-output-record)
                                              (stream timeline-pane)
                                              &optional
                                              region
                                              offset-x
                                              offset-y)
  (let ((offset (- (clim:bounding-rectangle-min-x (clim:untransform-region
                                                   (clim:sheet-native-transformation stream)
                                                   (clim:sheet-native-region stream)))
                   (clim:bounding-rectangle-min-x record))))
    (if (plusp offset)
        (let ((changes (make-hash-table :test #'eq)))
          (clim:map-over-output-records
           (lambda (child)
             (unless (typep child 'climi::draw-rectangle-output-record)
               (multiple-value-bind (x y) (clim:output-record-position child)
                 (setf (gethash child changes) (list x y))
                 (setf (clim:output-record-position child) (values (+ x offset) y)))))
           record)

          (call-next-method)

          (clim:map-over-output-records
           (lambda (child)
             (unless (typep child 'climi::draw-rectangle-output-record)
               (setf (clim:output-record-position child)
                     (values-list (gethash child changes)))))
           record))
        (call-next-method))))
