;;;; presentations.lisp --- Presentation types provided by the view.timeline module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.timeline)

;;; `thread' presentation type
;;;
;;; Represents a thread for which samples have been collected.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type thread (&optional (selected? t))
    :inherit-from '(t)))

#+later (flet ((call-with-thread-display (thunk stream thread selected?
                                  &key
                                  (length-limit 20))
         (let* ((maybe-name (name thread))
                (name       (or maybe-name "unnamed"))
                (text       (if (< (length name) length-limit)
                                name
                                (concatenate
                                 'string (subseq name 0 length-limit) "…"))))
           (clim:with-drawing-options (stream :ink       (if selected?
                                                             clim:+foreground-ink+
                                                             clim:+light-gray+)
                                              :text-face (if maybe-name
                                                             :bold
                                                             '(:bold :italic)))
             (funcall thunk stream text)))))

  (clim:define-presentation-method clim:present ((object thread)
                                                 (type   thread)
                                                 (stream t)
                                                 (view   t)
                                                 &key)
    (call-with-thread-display
     (lambda (stream text)
       (write-string text stream))
     stream object selected?))

  (clim:define-presentation-method clim:present ((object thread) ; TODO too similar to previous
                                                 (type   thread)
                                                 (stream t)
                                                 (view   timeline-view)
                                                 &key)
    (call-with-thread-display
     (lambda (stream text)
       (clim:draw-text* stream text 0 .5 :align-y :center))
     stream object selected?)))

;;; `trace' presentation type
;;;
;;; Represents a "call-stack snapshot" collected in a single thread.

(clim:define-presentation-type trace ()
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   trace)
                                               (stream t)
                                               (view   clim:textual-view)
                                               &key)
  (princ "<trace>" stream))

(clim:define-presentation-method clim:present ((object t ; model::standard-trace
                                                       )
                                               (type   trace)
                                               (stream t)
                                               (view   t)
                                               &key)
  (let ((start (- (model:time object)          0 ; *start-time*
                  ))
        (end   (- (+ (model:time object) .001) 0 ; *start-time*
                  )))
    (multiple-value-bind (dx dy) (clim:transform-distance
                                  (clim:medium-transformation stream) 3 3)
      (clim:draw-ellipse* stream start end 10 dx 0 0 dy))))

(clim:define-presentation-type traces ()
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   traces)
                                               (stream t)
                                               (view   clim:textual-view)
                                               &key)
  (princ "<traces>" stream))

(clim:define-presentation-method clim:present ((object t)
                                               (type   traces)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (multiple-value-bind (dx dy) (clim:transform-distance
                                (clim:medium-transformation stream) .01 .01)
    (let ((traces object)
          (dt 5))
      (if (< dx 3)
          (loop :with head = traces    ; TODO
                :for start :from (if (first head) (model:time (first head)) 0) :by dt
                :for end = (+ start dt)
                :for traces = (loop :for trace = (first head)
                                    :for time = (when trace (model:time trace))
                                    :while (and time (<= start time end))
                                    :do (pop head)
                                    :collect trace)
                :while head
                :do                    ; (maxf time-max end)
                   (clim:with-output-as-presentation (stream (first traces) 'trace)
                     (when traces
                       (clim:draw-rectangle* stream (- start (if (boundp '*start-time*) *start-time* start)) (- 20 #+later lane-height (/ (length traces) 20))
                                             (- end (if (boundp '*start-time*) *start-time* start)) 20 #+later lane-height
                                             :ink clim:+light-blue+))))
          (loop :for i :from 0
                :for trace :in traces
                :do  (let ((time (- (model:time trace) *start-time*)))
                       (when (plusp time) ; TODO hack
                         (clim:with-output-as-presentation (stream trace 'trace)
                           (clim:draw-ellipse* stream time (+ 4 (mod (* 4 i) 12)) .01 0 0 .01)))))
          #+no (loop :for (time . trace) :in traces
                     :with limit = (truncate (- lane-height 8) 4)
                     :for i :from 0
                     :do (maxf time-max time)
                         (clim:with-output-as-presentation (pane trace 'trace)
                           (let ((x (* time-scale time)))
                             (clim:draw-circle* pane x (+ 4 (* 4 (mod i limit))) 3))))))))

;;; Event

(clim:define-presentation-type event ()
  :inherit-from '(t))

(defun draw-event-info (event stream)
  (clim:with-drawing-options (stream :text-size :smaller)
    (princ (model:name event) stream)
    (terpri stream)
    (clim:formatting-table (stream)
      (loop :for (name value) :on (model::properties event) :by #'cddr
            :do (clim:formatting-row (stream)
                  (clim:formatting-cell (stream)
                    (format stream "~(~A~)" name))
                  (clim:formatting-cell (stream :align-x :right)
                    (format stream "~:D" value)))))))

(clim:define-presentation-method clim:highlight-presentation
    ((type   event)
     (record t)
     (stream clim:extended-output-stream)
     (state  t))
  (clim.flamegraph.view::call-as-highlighting
   (lambda (type record stream state)
     (declare (ignore type state))
     (let ((event (clim:presentation-object record)))
       (draw-event-info event stream)))
   type record stream state))

(flet ((draw-info (record event stream)
         (clim:with-bounding-rectangle* (x1 y1 x2 y2) record
           (clim:surrounding-output-with-border (stream :shape      :drop-shadow
                                                        :background clim:+background-ink+)
             (clim:with-end-of-line-action (stream :allow)
                                        ; (clim:draw-design stream )
               (setf (clim:stream-cursor-position stream) (values x1 y1))
               (clime:with-temporary-margins (stream :left `(:absolute ,x1) :top `(:absolute ,y1))
                 (clim:with-drawing-options (stream :text-size :smaller)
                   (princ (model:name event) stream)
                   (terpri stream)
                   (clim:formatting-table (stream)
                     (loop :for (name value) :on (model::properties event) :by #'cddr
                           :do (clim:formatting-row (stream)
                                 (clim:formatting-cell (stream)
                                   (format stream "~(~A~)" name))
                                 (clim:formatting-cell (stream :align-x :right)
                                   (format stream "~:D" value))))))))))))

  (clim:define-presentation-method clim:highlight-presentation
      ((type   event)
       (record t)
       (stream clim:extended-output-stream)
       (state  (eql :highlight)))
    (call-next-method)
    #+no (clim:with-output-recording-options (stream :draw t :record nil)
      (draw-info record (clim:presentation-object record) stream)))

  (clim:define-presentation-method clim:highlight-presentation
      ((type   event)
       (record t)
       (stream clim:extended-output-stream)
       (state  (eql :unhighlight)))
    (call-next-method)
    #+no (let ((record (clim:with-output-recording-options (stream :draw nil :record nil)
                         (clim:with-output-to-output-record (stream)
                           (draw-info record (clim:presentation-object record) stream)))))
           (clim:repaint-sheet stream record))))
