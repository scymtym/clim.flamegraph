;;;; presentations.lisp --- Presentations for standard model objects.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view)

;;; Names

(clim:define-presentation-type name (&key (count nil) (qualified? nil) (style :dark))
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   name)
                                               (stream t)
                                               (view   t)
                                               &key)
  (let* ((name-string (model::name-string object :qualified? qualified?))
         (string      (if count
                          (truncate-string name-string count)
                          name-string)))
    (write-string string stream)))

(clim:define-presentation-method clim:present ((object t)
                                               (type   name)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (clim:with-drawing-options (stream :ink (symbol-color object :style style))
    (call-next-method)))

(clim:define-presentation-method clim:present ((object model::standard-function)
                                               (type   name)
                                               (stream t)
                                               (view   t)
                                               &key)
  (clim:present (model:name #+TODO model::called-function object) type :stream stream :view view))

;;; Functions

(clim:define-presentation-type hit-count (&key (total-hit-count nil))
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object t)
                                               (type   hit-count)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (format stream "~:D" object)
  (when total-hit-count
    (let ((ratio (min (/ object total-hit-count) 1))) ; TODO hack
      (clim:with-drawing-options (stream :text-size :smaller :ink (clim:make-rgb-color ratio 0 0))
        #-mezzano (format stream " (~,2,2F %)" ratio)
        #+mezzano (format stream " (~F %)" (float (* 100 ratio) 1.0))))))

(clim:define-presentation-type call-statistics (&key (total-hit-count nil))
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object model::standard-function)
                                               (type   call-statistics)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (let ((x (clim:stream-cursor-position stream))) ; TODO macro
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream)
          (write-string "Total hits" stream))
        (clim:formatting-cell (stream :align-x :right)
          (clim:present (model::hit-count object)
                        `(hit-count :total-hit-count ,total-hit-count)
                        :stream stream :view view)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream)
          (write-string "Total calls" stream))
        (clim:formatting-cell (stream :align-x :right)
          (format stream "~:D" (model::call-count object))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream)
          (write-string "Total time" stream))
        (clim:formatting-cell (stream :align-x :right)
          (om:print-human-readable-duration stream (model::total-run-time object)))))
    (setf (clim:stream-cursor-position stream) (values x (+ (nth-value 1 (clim:stream-cursor-position stream)) 8))))
  (format stream "Called from threads~%")
  (clim:format-items (model::calling-threads object)
                     :printer (lambda (item stream)
                                (let ((string (truncate-string (princ-to-string item) 40)))
                                  (write-string string stream)))
                     :stream stream :n-columns 1))  ; TODO thread presentation type

(clim:define-presentation-type called-function (&key (style :dark)))

(clim:define-presentation-method clim:present ((object t)
                                               (type   called-function)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (clim:present (model:name object) `(name ,style) :stream stream :view view)
  (fresh-line stream)
  (clim:present (model:name #+TODO model::called-function object) 'call-statistics
                :stream stream :view view))
