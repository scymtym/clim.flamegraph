;;;; timeline.lisp --- Timeline view and presentations.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; `timeline-view'

(defclass timeline-view (clim:view)
  ((time-scale :initarg  :time-scale
               :reader   time-scale)
   (selection  :initarg  :selection
               :reader   selection)))

;; TODO make this a value-gadget. trace list is the value

(clim:define-presentation-type interval ())

(defconstant +interval-ink+
  (if (boundp '+interval-ink+)
      (symbol-value '+interval-ink+)
      (clim:compose-in clim:+red+ (clim:make-opacity .3))))

(clim:define-presentation-method clim:present ((object interval)
                                               (type   interval)
                                               (stream t)
                                               (view   timeline-view)
                                               &key)
  (let ((scale (time-scale view)))
    (with-accessors ((start start) (end end)) object
      (clim:draw-rectangle* stream (* scale start) 0 (* scale end) 1
                            :ink +interval-ink+))))

(flet ((update-selection (frame interval)
         (with-accessors ((start start) (end end)) interval
           (setf (selected-traces frame)
                 (select-traces (traces frame)
                                :start-time start :end-time end)))))

  (define-flamegraph-command (com-move-interval :name t)
      ((interval interval) (offset real))
    (incf (start interval) offset)
    (incf (end   interval) offset)

    (update-selection clim:*application-frame* interval))

  (clim:define-drag-and-drop-translator drag-interval
      (interval command interval flamegraph)
      (object presentation x y)
    (declare (ignore y))
    `(com-move-interval ,object ,(/ (- x (clim:bounding-rectangle-min-x presentation)) 300)))

  (define-flamegraph-command (com-set-interval-start :name t)
      ((interval interval) (start real))
    (setf (start interval) start)

    (update-selection clim:*application-frame* interval))

  (define-flamegraph-command (com-set-interval-end :name t)
      ((interval interval) (end real))
    (setf (end interval) end)

    (update-selection clim:*application-frame* interval)))

;;; `thread' class and presentation type
;;;
;;; Represents a thread for which samples have been collected.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass thread ()
    ((thread    :initarg  :thread
                :reader   thread)
     #+no (selected? :initarg  :selected?
                :accessor selected?
                :initform t)))

  (clim:define-presentation-type thread ()))

(defmethod name ((object thread))
  (sb-thread:thread-name (thread object)))

(clim:define-presentation-method clim:present ((object thread)
                                               (type   thread)
                                               (stream t)
                                               (view   timeline-view)
                                               &key)
  (let* ((name         (name object))
         (display-name (or name "unnamed")))
    (clim:with-drawing-options (stream :ink (if (selected? (thread object) (selection view))
                                                clim:+foreground-ink+
                                                clim:+red+ ; +lightgray+
                                                )
                                       :text-face (if name
                                                      :bold
                                                      '(:bold :italic)))
      (clim:draw-text* stream display-name 0 .5 :align-y :center))))

(define-flamegraph-command toggle-thread ((thread thread))
  ; (setf (selected? thread) (not (selected? thread)))
  )

(clim:define-presentation-to-command-translator toggle-thread
    (thread toggle-thread flamegraph)
    (object)
  `(,object))

;;; `trace' presentation type
;;;
;;; Represents a "call-stack snapshot" collected in a single thread.

(clim:define-presentation-type trace ())

(clim:define-presentation-method clim:present ((object t)
                                               (type   trace)
                                               (stream t)
                                               (view   clim:textual-view)
                                               &key)
  (princ "<trace>" stream))

;;; Helper class for presenting the traces collected for a given
;;; thread in a chronological fashion.
(defclass lane ()
  ((index  :initarg  :index
           :type     non-negative-integer
           :reader   index)
   (thread :initarg  :thread
           :type     thread
           :reader   thread)
   (traces :type     list
           :accessor traces
           :initform '())))

(defun make-lane (index thread)
  (make-instance 'lane :index index :thread (make-instance 'thread :thread thread)))

(defun display-timeline (frame pane &key (time-scale 300) (lane-height 24))
  (let* ((traces    (traces frame))
         (selection (selection pane))
         (interval  (interval selection))
         (view      (clim:stream-default-view pane))

         (lanes     '()))
    ;; make a function traces -> selection -> filtered-traces

    ;; TODO separate function for lane precipitation
    ;; Create a lanes for each thread and collect associated traces.
    (let ((index -1))
      (sb-sprof:map-traces
       (lambda (thread time trace)
         (let ((lane (or (find thread lanes :key (compose #'thread #'thread)) ; sorry
                         (let ((new (make-lane (incf index) thread)))
                           (push new lanes)
                           new))))
           (push (cons time trace) (traces lane))))
       traces))
    (setf lanes (nreverse lanes))

    ;; Draw lanes from top to bottom.
    (let ((thread-max-x 0)
          (time-max     0))
      (clim:with-translation (pane 0 16)

        (map nil (lambda (lane)
                   (with-accessors ((index index) (thread thread)) lane
                     (clim:with-translation (pane 0 (* lane-height index))
                       (clim:with-scaling (pane 1 (- lane-height 4))
                         (let ((presentation (clim:present thread 'thread :stream pane :view view)))
                           (maxf thread-max-x (clim:bounding-rectangle-max-x presentation)))))))
             lanes)

        (clim:with-translation (pane (+ thread-max-x 4) 0)
          (map nil (lambda (lane)
                     (with-accessors ((index index) (traces traces)) lane
                       (clim:with-translation (pane 0 (* lane-height index))
                         (map nil (lambda (time+trace)
                                    (destructuring-bind (time . trace) time+trace
                                      (maxf time-max time)
                                      (clim:with-output-as-presentation (pane trace 'trace)
                                        (let ((x (* time-scale time)))
                                          (clim:draw-line* pane x 2 x (- lane-height 4))))))
                              traces))))
               lanes)

          ;;
          (clim:with-scaling (pane 1 (* lane-height (length lanes)))
            (clim:present interval 'interval :stream pane :view view))))

      (clim:with-translation (pane (+ thread-max-x 4) 0)
        (loop :for i :from 0 :to (floor time-max)
              :for label = (format nil "~D s" i)
              :do (clim:draw-text* pane label (* i time-scale) 0 :align-y :top))))

    (clim:change-space-requirements pane :resize-frame nil)))

;;;

(defclass timeline-pane (clim:application-pane)
  ((selection :reader   selection
              :initform (make-selection 1 3))) ; TODO where should the selection be?
  (:default-initargs
   :display-function 'display-timeline
   :default-view     (make-instance 'timeline-view :time-scale 300 :selection (make-selection 1 3))))
