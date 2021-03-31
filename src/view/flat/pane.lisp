(cl:in-package #:clim.flamegraph.view.flat)

(defvar *column-name*
  (make-instance 'sortable-column-information
                 :name "Name"
                 :test #'string<
                 :key  #'model::name-string))

(defvar *column-total-hits*
  (make-instance 'sortable-column-information
                 :name "Total hits"
                 :test #'<
                 :key  #'model::hit-count))

(defvar *column-total-non-recursive-hits*
  (make-instance 'sortable-column-information
                 :name "Total non-recursive hits"
                 :test #'<
                 :key  #'model::non-recursive-hit-count))

(defvar *column-self-hits*
  (make-instance 'sortable-column-information
                 :name  "Self hits"
                 :test  #'<
                 :key   #'model::self-hit-count))

(defclass flat-pane (clim:application-pane)
  ((%model :initarg :model
           ;; :type model:standard-run
           :reader  model))
  (:default-initargs
   :default-view     (make-instance 'table-view :columns (list *column-name*
                                                               *column-total-hits*
                                                               *column-total-non-recursive-hits*
                                                               *column-self-hits*))
   :display-function 'display))

(defun display (frame pane)
  (declare (ignore frame))
  (clim:present (model pane) 'function-table :stream pane))

(clim:define-command-table command-table)

(clim:define-command (com-set-sort-column :command-table command-table :name t)
    ((view   t)
     (column sortable-column-information))
  (let ((old-column (sort-column view)))
    (if (eq old-column column)
        (setf (sort-direction view) (ecase (sort-direction view)
                                      (:ascending  :descending)
                                      (:descending :ascending)))
        (setf (sort-column    view) column
              (sort-direction view) :ascending))))

(clim:define-presentation-to-command-translator column->set-sort-column
    (sortable-column-information com-set-sort-column command-table)
    (object presentation)
  (list (climi::presentation-view presentation) object))

(clim:define-command (com-select-function :command-table command-table :name t)
    ((name view::name :gesture :select))
  )
