;;;; presentations.lisp --- Presentation for the flat function list.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view.flat)

(defclass table-view (clim:view)
  ((%columns        :initarg  :columns
                    :type     list
                    :reader   columns)
   (%sort-column    :initarg  :sort-column
                    :accessor sort-column
                    :initform nil)
   (%sort-direction :initarg  :sort-direction
                    :accessor sort-direction
                    :initform :ascending)))

(defclass column-information ()
  ((%name :initarg :name
          :reader  name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sortable-column-information (column-information)
    ((%test :initarg  :test
            :reader   test)
     (%key  :initarg  :key
            :reader   key))))

(clim:define-presentation-method clim:present ((object sortable-column-information)
                                               (type   sortable-column-information)
                                               (stream clim:extended-output-stream)
                                               (view   table-view)
                                               &key)
  (when (eq object (sort-column view))
    (ecase (sort-direction view)
      (:ascending  (write-string "↓" stream))
      (:descending (write-string "↑" stream))))
  (write-string (name object) stream))

(clim:define-presentation-type function-table ()
  :inherit-from '(t))

(clim:define-presentation-method clim:present ((object model::standard-run)
                                               (type   function-table)
                                               (stream clim:extended-output-stream)
                                               (view   t)
                                               &key)
  (let* ((trace-count  (length (model:traces object)))
         #+why? (sample-count (reduce #'+ (model::traces object)
                               :key (lambda (trace)
                                      (if (compute-applicable-methods
                                           #'model::samples (list trace))
                                          (length (model::samples trace))
                                          0))))
         (functions    (model:functions object))
         (sort-column  (sort-column view))
         (sort-test    (when sort-column
                         (ecase (sort-direction view)
                           (:ascending  (test sort-column))
                           (:descending (let ((test (test sort-column)))
                                          (lambda (left right)
                                            (not (funcall test left right))))))))
         (sort-key     (when sort-column
                         (key sort-column)))
         (sorted       (if (and sort-test sort-key)
                           (sort (copy-seq functions) sort-test :key sort-key)
                           functions)))
    (clim:with-end-of-line-action (stream :allow)
      (clim:formatting-table (stream :x-spacing 8)
        (clim:with-drawing-options (stream :text-face :bold)
          (clim:formatting-row (stream)
            (map nil (lambda (column)
                       (clim:formatting-cell (stream)
                         (clim:present column 'sortable-column-information
                                       :stream stream :view view)))
                 (columns view))))

        (let ((i 0))
          (map nil (lambda (function)
                     (clim:surrounding-output-with-border
                         (stream :ink        clim:+transparent-ink+
                                 :background (if (zerop (mod (incf i) 2))
                                                 nil
                                                 clim:+grey94+)
                                 :padding    0)
                       (clim:formatting-row (stream)
                         (clim:formatting-cell (stream)
                           (clim:present function `(view::name :count 50 :qualified? t)
                                         :stream stream :view view)
                           #+no (clim:with-drawing-options (stream :text-family :fix
                                                                   :ink         (view:symbol-color
                                                                                 (model:name function)))
                                  (let ((string (view::truncate-string (model::name-string function) 20)))
                                    (write-string string stream))))

                         (clim:formatting-cell (stream :align-x :right)
                           (clim:present (model::hit-count function) `(view::hit-count :total-hit-count ,trace-count)
                                         :stream stream :view view))

                         (clim:formatting-cell (stream :align-x :right)
                           (clim:present (model::non-recursive-hit-count function) `(view::hit-count :total-hit-count ,trace-count)
                                         :stream stream :view view))

                         (clim:formatting-cell (stream :align-x :right)
                           (clim:present (model::self-hit-count function) `(view::hit-count :total-hit-count ,trace-count)
                                         :stream stream :view view)))))
               sorted))))))
