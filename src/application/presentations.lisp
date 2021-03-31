;;;; presentations.lisp --- Presentations used in the application.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.application)

(defclass configuration-view (clim:view) ())

;;; `configuration-node'

(clim:define-presentation-type configuration-node ()
  :inherit-from t)

;;; `thread'

(clim:define-presentation-type thread ()
  :inherit-from t)

(clim:define-presentation-method clim:present ((object t)
                                               (type   thread)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (format stream "Thread ~A"
          (view::truncate-string (bt:thread-name object) 20)))

(clim:define-presentation-type non-configured-thread ()
  :inherit-from 'thread)

(clim:define-presentation-type configured-thread (&key (configured-threads '()))
  :inherit-from '(and configuration-node thread))

(defmethod node-presentation-type ((node sb-thread:thread))
  'configured-thread)

;;; `source'

(defmethod node-presentation-type ((node t))
  'source)

(clim:define-presentation-type source (&key (configured-sources '()))
  :inherit-from 'configuration-node)

(clim:define-presentation-method clim:present ((object t)
                                               (type   source)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (princ object stream)))

;;; Statistical profiler source

(defmethod node-presentation-type ((node clim.flamegraph.backend.sb-sprof:source))
  'statistical-profiler-source)

(clim:define-presentation-type statistical-profiler-source ()
  :inherit-from '(source))

(clim:define-presentation-method clim:present ((object t)
                                               (type   statistical-profiler-source)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (write-string "Statistical Profiler" stream))
  (terpri stream)
  (flet ((property (name reader &optional note) ; TODO repeated in advice source
           (clim:formatting-row (stream)
             (clim:formatting-cell (stream)
               (write-string name stream))
             (clim:formatting-cell (stream)
               (if-let ((value (funcall reader object)))
                 (typecase value
                   (integer (format stream "~:D" value))
                   (t       (princ value stream)))
                 (clim:with-drawing-options (stream :text-face :italic)
                   (write-string "none" stream))))
             (when note
               (clim:formatting-cell (stream)
                 (clim:with-drawing-options (stream :ink clim:+gray40+ :text-size :smaller)
                   (write-string note stream)))))))
    (clim:formatting-table (stream)
      (property "Sample Interval"   #'clim.flamegraph.backend.sb-sprof::sample-interval "s")
      (property "Trace Depth Limit" #'clim.flamegraph.backend.sb-sprof::trace-depth-limit)
      (property "Name Test"         #'clim.flamegraph.backend.sb-sprof::name-test))))

;;; Advice source

(defmethod node-presentation-type ((node clim.flamegraph.backend.advice:source))
  'advice-source)

(clim:define-presentation-type advice-source ()
  :inherit-from '(source))

(clim:define-presentation-method clim:present ((object t)
                                               (type   advice-source)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (write-string "Deterministic Profiler" stream))
  (terpri stream)
  (flet ((property (name reader &optional note)
           (clim:formatting-row (stream)
             (clim:formatting-cell (stream)
               (write-string name stream))
             (clim:formatting-cell (stream)
               (if-let ((value (funcall reader object)))
                 (typecase value
                   (integer (format stream "~:D" value))
                   (t       (princ value stream)))
                 (clim:with-drawing-options (stream :text-face :italic)
                   (write-string "none" stream))))
             (when note
               (clim:formatting-cell (stream)
                 (clim:with-drawing-options (stream :ink clim:+gray40+ :text-size :smaller)
                   (write-string note stream)))))))
    (clim:formatting-table (stream)
      (property "Trace Depth Limit"       #'clim.flamegraph.backend.advice::depth-limit)
      (property "Sample Rate"             #'clim.flamegraph.backend.advice::sample-rate "ratio")
      (property "Minimal Duration"        #'clim.flamegraph.backend.advice::min-duration "s")
      (property "Per-thread Sample Limit" #'clim.flamegraph.backend.advice::count-limit))))

(defmethod node-presentation-type ((node cons))
  'advice-specification)

(clim:define-presentation-type advice-specification ()
  :inherit-from 'configuration-node)

(clim:define-presentation-method clim:present ((object t)
                                               (type   advice-specification)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (destructuring-bind (subject arguments) object
    (typecase subject
      ((eql :blockers) (format stream "Blocking Functions"))
      ((eql :io)       (format stream "IO Functions"))
      (symbol          (format stream "Function ~S" subject))
      (package         (format stream "Package ~A" (package-name subject))))
    (when arguments
      (write-string " with arguments" stream))))

;;; Memory source

(defmethod node-presentation-type ((node clim.flamegraph.backend.sb-memory:source))
  'memory-source)

(clim:define-presentation-type memory-source ()
  :inherit-from '(source))

(clim:define-presentation-method clim:present ((object t)
                                               (type   memory-source)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (write-string "Memory Monitor" stream)))

;;; `configuration-root'

(clim:define-presentation-type configuration-root ()
  :inherit-from t)

(clim:define-presentation-method clim:present ((object t)
                                               (type   configuration-root)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (write-string "Recording" stream)))

(defmethod node-presentation-type ((node source-configuration))
  'configuration-root)

;;; `source-configuration'

(clim:define-presentation-method clim:present ((object source-configuration)
                                               (type   source-configuration)
                                               (stream clim:extended-output-stream)
                                               (view   configuration-view)
                                               &key)
  (clim:format-graph-from-root
   object
   (lambda (node stream)
     (let ((type (node-presentation-type node)))
       (clim:surrounding-output-with-border (stream)
         (clim:present node type :stream stream :view view :single-box t))))
   (lambda (node)
     (typecase node
       (source-configuration (sources node))
       (clim.flamegraph.backend.sb-sprof:source
        *threads*)
       (clim.flamegraph.backend.advice:source
        (append (clim.flamegraph.backend.advice::specification node)
                *threads*))))
   :stream stream :arc-drawer (lambda (stream from to x1 y1 x2 y2)
                                (clim:draw-arrow* stream x2 y2 x1 y1))
   :merge-duplicates t :duplicate-test #'eq :maximize-generations t))

;;; Run selection

(defclass run-list-view (clim:view) ())

(clim:define-presentation-type run (&key (selected? nil))
  :inherit-from '(t))

(defun print-run-time (run stream)
  (multiple-value-bind (second minute hour)
      (decode-universal-time (floor (model:start-time run)))
    (format stream "~2,'0D:~2,'0D:~2,'0D"
            hour minute second)))

(clim:define-presentation-method clim:present ((object t)
                                               (type   run)
                                               (stream clim:extended-output-stream)
                                               (view   run-list-view)
                                               &key)
  (flet ((do-it (stream)
           (let ((name (model:name object)))
             (cond ((null name)
                    (clim:with-drawing-options (stream :text-face :bold)
                      (print-run-time object stream)))
                   (t
                    (clim:with-drawing-options (stream :text-face :bold)
                      (princ name stream))
                    (write-char #\Space stream)
                    (clim:with-drawing-options (stream :text-face :italic :text-size :smaller)
                      (print-run-time object stream)))))
           (terpri stream)
           (clim:with-drawing-options (stream :text-face :italic :text-size :smaller)
             (format stream "~5,F seconds~%"  (model::duration object))
             (format stream "~:D thread~:P~%" (length (model::threads object)))
             (format stream "~:D trace~:P"    (length (model::traces object))))))
    (clim:surrounding-output-with-border
     (stream :shape       :rounded
             :background  (if selected? clim:+light-blue+ clim:+gray90+)
             :outline-ink (if selected? clim:+dark-blue+ clim:+gray60+))
     (do-it stream))))
