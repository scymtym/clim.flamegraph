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
   #+no (selection  :initarg  :selection
               :reader   selection)))

;; TODO make this a value-gadget. trace list is the value

(clim:define-presentation-type interval ())

(defconstant +interval-ink+
  (if (boundp '+interval-ink+)
      (symbol-value '+interval-ink+)
      (clim:compose-in clim:+black+ (clim:make-opacity .1))))

(clim:define-presentation-method clim:present ((object interval)
                                               (type   interval)
                                               (stream t)
                                               (view   timeline-view)
                                               &key)
  (let ((scale (time-scale view)))
    (with-accessors ((start start) (end end)) object
      (clim:draw-rectangle* stream (* scale start) 0 (* scale end) 1
                            ;; :ink +interval-ink+
                            :filled         nil
                            :ink            clim:+dark-gray+
                            :line-thickness 2
                            :line-dashes    #(2 2)))))

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

;;; `thread' presentation type
;;;
;;; Represents a thread for which samples have been collected.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type thread (&optional (selected? t))))

(clim:define-presentation-method clim:present ((object thread)
                                               (type   thread)
                                               (stream t)
                                               (view   t)
                                               &key)
  (let* ((name         (name object))
         (display-name (or name "unnamed")))
    (clim:with-drawing-options (stream :ink       (if selected?
                                                      clim:+foreground-ink+
                                                      clim:+light-gray+)
                                       :text-face (if name
                                                      :bold
                                                      '(:bold :italic)))
      (write-string display-name stream))))

(clim:define-presentation-method clim:present ((object thread) ; TODO too similar to previous
                                               (type   thread)
                                               (stream t)
                                               (view   timeline-view)
                                               &key)
  (let* ((name         (name object))
         (display-name (or name "unnamed")))
    (clim:with-drawing-options (stream :ink       (if selected?
                                                      clim:+foreground-ink+
                                                      clim:+light-gray+)
                                       :text-face (if name
                                                      :bold
                                                      '(:bold :italic)))
      (clim:draw-text* stream display-name 0 .5 :align-y :center))))

(define-flamegraph-command toggle-thread ((thread thread))
  (let ((selection (selection (clim:find-pane-named clim:*application-frame* 'timeline))))
    (setf (selected? thread selection) (not (selected? thread selection)))))

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

(defvar *thread-hack* (make-hash-table :test #'eq))

(defun make-lane (index thread)
  (make-instance 'lane :index index :thread (ensure-gethash thread *thread-hack* (make-instance 'thread :thread thread))))

(defun display-timeline (frame pane
                         &key
                         (time-scale (time-scale (clim:stream-default-view pane)))
                         (lane-height 24))
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
                     (let ((selected? (selected? thread selection)))
                       (clim:with-translation (pane 0 (+ (* lane-height index) (/ lane-height 2)))
                         (let ((presentation (clim:present thread `(thread ,selected?) :stream pane :view view)))
                           (maxf thread-max-x (clim:bounding-rectangle-max-x presentation)))))))
             lanes)

        (clim:with-translation (pane (+ thread-max-x 4) 0)
          (loop :for lane :in lanes
                :for i :from 0
                :for ink = (if (selected? (thread lane) selection)
                               (clim:make-contrasting-inks 8 (1+ (mod i 7)))
                               clim:+light-gray+)
                :do (clim:with-drawing-options (pane :ink ink)
                      (with-accessors ((index index) (traces traces)) lane
                        (clim:with-translation (pane 0 (* lane-height index))
                          (loop :for (time . trace) :in traces
                                :with limit = (truncate (- lane-height 4) 4)
                                :for i :from 0
                                :do (maxf time-max time)
                                    (clim:with-output-as-presentation (pane trace 'trace)
                                      (let ((x (* time-scale time)))
                                        (clim:draw-circle* pane x (+ 2 (* 4 (mod i limit))) 4))))))))

          ;;
          (clim:with-scaling (pane 1 (* lane-height (length lanes)))
            (clim:present interval 'interval :stream pane :view view)))

        (loop :for lane :in (rest lanes)
              :for y :from lane-height :by lane-height
              :do (clim:draw-line* pane 0 y (+ thread-max-x 4 (* time-max time-scale)) y
                                   :line-style (clim:make-line-style :dashes '(4 4)))))

      (clim:with-translation (pane (+ thread-max-x 4) 0)
        (loop :for i :from 0 :to (floor time-max)
              :for label = (format nil "~D s" i)
              :do (clim:draw-text* pane label (* i time-scale) 0 :align-y :top))))

    (clim:change-space-requirements pane :resize-frame nil)))

;;;

(defclass timeline-pane (clim:application-pane)
  ((selection :initarg  :selection
              :reader   selection
              :initform (make-selection 1 3))) ; TODO where should the selection be?
  (:default-initargs
   :display-function 'display-timeline
   :default-view     (make-instance 'timeline-view :time-scale 100 ; unused :selection (make-selection 1 3)
                                    )))
