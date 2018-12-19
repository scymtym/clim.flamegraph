;;;; flamegraph.lisp --- Flamegraph view and presentation.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

;;; Flame graph view state (also serves as view in the Clim sense)

(defclass flamegraph-state (clim:view)
  (;; Data
   (%tree        :initarg  :tree
                 :accessor tree
                 :initform nil)
   (%thread-tree :initarg  :thread-tree
                 :accessor thread-tree
                 :initform nil)
   (%root        :initarg  :root
                 :accessor root
                 :writer   (setf %root)
                 :initform nil)
   ;; Visual state
   (%scale       :initarg  :scale
                 :accessor scale
                 :initform 0)
   (%depth-limit :initarg  :depth-limit
                 :accessor depth-limit
                 :writer   (setf %depth-limit)
                 :initform 100)
   ;; Cache
   (%text-style  :initarg  :text-style
                 :accessor text-sytle)
   (%char-width  :accessor char-width
                 :initform nil)
   ;;
   (%hook        :initarg  :hook
                 :type     (or null function)
                 :accessor hook
                 :initform nil)))

(defmethod shared-initialize :after ((instance   flamegraph-state)
                                     (slot-names t)
                                     &key
                                     (tree nil tree-supplied-p)
                                     (root nil root-supplied-p))
  (declare (ignore root))
  (when (and tree-supplied-p (not root-supplied-p))
    (setf (%root instance) tree)))

(defmethod (setf tree) :after ((new-value t) (object flamegraph-state))
  (setf (%root object) nil)
  (when-let ((hook (hook object)))
    (funcall hook object)))

(defmethod (setf root) :after ((new-value t) (object flamegraph-state))
  (when-let ((hook (hook object)))
    (funcall hook object)))

(defmethod (setf scale) :after ((new-value t) (object flamegraph-state))
  (when-let ((hook (hook object)))
    (funcall hook object)))

(defmethod (setf depth-limit) :after ((new-value t) (object flamegraph-state))
  (when-let ((hook (hook object)))
    (funcall hook object)))

(defmethod (setf text-style) :after ((new-value t) (object flamegraph-state))
  (setf (char-width object) nil))

;;; Presentation types

(defun name+depth+position->color (name depth position)
  (declare (ignore depth position))
  (symbol-color name))

(clim:define-presentation-type call
    (&optional (depth 0) (position 0) (scale 1)))

(clim:define-presentation-method clim:present ((object node)
                                               (type   call)
                                               stream
                                               view
                                               &key)
  (format stream "~A, ~D hit~:P"
          (node-name object) (node-count object)))

(clim:define-presentation-type call-tree (&optional (shrink 1)))

(clim:define-presentation-method clim:present ((object node)
                                               (type   call-tree)
                                               stream
                                               view
                                               &key)
  (write-string "<call tree>" stream))

;;; Flamegraph view

(clim:define-presentation-method clim:present ((object node)
                                               (type   call)
                                               stream
                                               (view   flamegraph-state)
                                               &key)
  (with-accessors ((count node-count) (call node-call)) object
    (multiple-value-bind (x y) (clim:stream-cursor-position stream)
      (let* (;; Text
             (name       (when call
                           (sb-sprof::node-name call)))
             (text       (typecase name
                           (null   "<root>")
                           (symbol (symbol-name name))
                           (t      (let ((*print-pretty* nil))
                                     (princ-to-string name)))))
             (length     (length text))
             ;; Text
             (style      (load-time-value
                          (clim:make-text-style :fix nil :small) t))

             (char-width (or (char-width view)
                             (setf (char-width view)
                                   (clim:text-size stream "W" :text-style style))))
             ;; Dimensions
             (width      (* scale count))
             (height     (nth-value 1 (clim:text-size stream text))) ; TODO should be constant now
             (text       (cond ((< width (* 2 char-width))
                                nil)
                               ((> (* length char-width) width)
                                (let ((end (1- (truncate width char-width))))
                                  (concatenate 'string (subseq text 0 end) "…")))
                               (t
                                text)))
             (background (name+depth+position->color
                          name depth position)))
        (clim:draw-rectangle* stream  x y (+ x width) (+ y height) :ink background)
        (when text
          (clim:draw-text* stream text (+ x (/ width 2)) (+ y (/ height 2))
                           :align-x :center :align-y :center
                           :text-style style)))

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
                 (values x (nth-value 1 (stream-cursor-position stream)))))))

(defvar *highlight-text-style*
  (clim:make-text-style :fix nil :small))

(labels ((info-text (node)
           (let ((*print-right-margin* nil)
                 (*print-miser-width*  nil))
             (format nil "~S, ~:D hit~:P"
                     (sb-sprof::node-name (node-call node))
                     (node-count node))))
         (draw-info (stream record node)
           (let* ((style *highlight-text-style*)
                  (text  (info-text node)))
             (clim:with-bounding-rectangle* (x1 y1 x2 y2) record
               (clim:surrounding-output-with-border (stream :shape      :drop-shadow
                                                            :background clim:+background-ink+)
                 ;; Include the RECORD so the border will be at least
                 ;; as big.
                 (clim:draw-design stream record :ink clim:+transparent-ink+)
                 (clim:draw-text* stream text
                                  (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                                  :align-x :center :align-y :center :text-style style))))))

  (clim:define-presentation-method clim:highlight-presentation ((type   call)
                                                                (record t)
                                                                (stream t)
                                                                (state  (eql :highlight)))
    (clim:with-output-recording-options (stream :draw t :record nil)
      (draw-info stream record (clim:presentation-object record))))

  (clim:define-presentation-method clim:highlight-presentation ((type   call)
                                                                (record t)
                                                                (stream t)
                                                                (state  (eql :unhighlight)))
    (let ((record (clim:with-output-recording-options (stream :draw nil :record nil)
                    (clim:with-output-to-output-record (stream)
                      (draw-info stream record (clim:presentation-object record))))))
      (clim:repaint-sheet stream record))))

(clim:define-presentation-method clim:present ((object node)
                                               (type   call-tree)
                                               stream
                                               (view   flamegraph-state)
                                               &key)
  (let* ((depth-limit (or (depth-limit view) most-positive-fixnum))
         (total-count (node-count object)) ; root node
         (sheet-width (clim:bounding-rectangle-width (clim:sheet-region stream)))
         (scale       (* sheet-width shrink (/ total-count) (expt 2 (scale view)))))
    (labels ((present-node (node &optional (depth 0) (position 0))
               (let* ((presentation (clim:present node `(call ,depth ,position ,scale)
                                                  :stream stream :view view :single-box t))
                      (x            (clim:bounding-rectangle-min-x presentation))
                      (y            (clim:bounding-rectangle-max-y presentation))
                      (position     0))
                 (setf (clim:stream-cursor-position stream) (values x y))
                 (when (< depth depth-limit)
                   (map nil (lambda (child)
                              (let* ((child-presentation
                                       (present-node child (1+ depth) (incf position)))
                                     (x2 (clim:bounding-rectangle-max-x child-presentation)))
                                (setf (clim:stream-cursor-position stream)
                                      (values x2 y))))
                        (hash-table-values (node-children node)))) ; TODO interface without hash-table
                 presentation)))
      (present-node object))))

(clim:define-presentation-method clim:present ((object thread-tree)
                                               (type   call-tree)
                                               stream
                                               (view   flamegraph-state)
                                               &key)
  (let ((total-count 0))
    (maphash (lambda (thread tree)
               (declare (ignore thread))
               (incf total-count (node-count tree)))
             (thread-tree-children object))
    (maphash (lambda (thread tree)
               (let ((shrink (/ (node-count tree) total-count)))
                 (let ((old-x (clim:stream-cursor-position stream)))
                   (clim:stream-increment-cursor-position stream 0 18)
                   (clim:present tree `(call-tree ,shrink)
                                 :stream stream :view view)
                   (let ((new-x (clim:stream-cursor-position stream)))
                     (setf (clim:stream-cursor-position stream)
                           (values (/ (+ old-x new-x) 2) 2))
                     (clim:present thread 'thread :stream stream :view view)
                     (setf (clim:stream-cursor-position stream)
                           (values (+ new-x 16) 0))))))
             (thread-tree-children object))))

;;; Pane

(defclass flamegraph-pane (clim:application-pane)
  ((%state :initarg  :state
           :reader   state
           :initform (make-instance 'flamegraph-state)))
  (:default-initargs
   :display-time       t
   :display-function   'display-flame-graph
   :end-of-line-action :scroll
   :end-of-page-action :scroll))

(defmethod initialize-instance :after ((instance flamegraph-pane) &key)
  (setf (hook (state instance))
        (lambda (state)
          (declare (ignore state))
          (clim:redisplay-frame-pane (clim:pane-frame instance) instance
                                     :force-p t))))

(defun display-flame-graph (frame pane)
  (let ((state (state pane)))
    (when-let ((tree (or (root        state)
                         (thread-tree state))))
      (clim:present tree 'call-tree :stream pane :view state)
      (clim:change-space-requirements pane :resize-frame nil))))

;; TODO this is a recurring pattern
(defmethod clim:redisplay-frame-pane :around ((frame clim:application-frame)
                                              (pane  flamegraph-pane)
                                              &key force-p)
  (declare (ignore force-p))
  (let ((viewport (clim:sheet-parent pane)))
    (multiple-value-bind (x-displacement y-displacement)
        (clim:transform-position (clim:sheet-transformation pane) 0 0)
      (call-next-method)
      (clim:scroll-extent
       pane
       (min (- x-displacement)
            (- (clim:bounding-rectangle-width pane)
               (clim:bounding-rectangle-width viewport)))
       (min (- y-displacement)
            (- (clim:bounding-rectangle-height pane)
               (clim:bounding-rectangle-height viewport)))))))
