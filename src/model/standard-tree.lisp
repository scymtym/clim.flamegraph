;;;; standard-tree.lisp --- Default implementations of the tree and node protocols
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

;;; `standard-tree'

(defclass standard-tree ()
  ())

;;; `standard-node'

(defclass standard-node (name-mixin
                         print-items:print-items-mixin)
  ((%children  :initarg  :children
               :reader   %children
               :initform (make-hash-table :test #'equal)) ; TODO start with list
   (%hit-count :initarg  :hit-count
               :reader   hit-count
               :accessor hit-count
               :initform 0)))

(defmethod print-items:print-items append ((object standard-node))
  `((:hit-count ,(hit-count object) " ~,D hit~:P" ((:after :name)))))

(defmethod find-child ((name t) (node standard-node))
  (gethash name (%children node)))

(defmethod (setf find-child) ((new-value t) (name t) (node standard-node))
  (setf (gethash name (%children node)) new-value))

(defmethod ensure-child ((name t) (node standard-node) (thunk function))
  (ensure-gethash name (%children node) (funcall thunk)))

;;;

#+unused (defmethod add-node! (tree path hit-count)
  (labels ((rec (node path)
             (when-let ((segment (first path)))
               (incf (hit-count node) hit-count)
               (rec (ensure-child
                     segment node
                     (lambda () (make-instance 'standard-node :name segment)))
                    (rest path)))))
    (rec tree path)))

(defmethod add-trace! (tree trace)
  (let ((node tree))
    (incf (hit-count node))
    (map-samples
     (lambda (sample)
       (let* ((name  (name sample))
              (child (ensure-child name node
                                   (lambda ()
                                     (make-instance 'standard-node
                                                    :name name)))))
         (incf (hit-count child))
         (setf node child)))
     trace)
    tree))

(defmethod add-run! (tree run)
  (map-traces (lambda (trace)
                (when (typep trace 'standard-trace) ; TODO HACK
                  (add-trace! tree trace)))
              #+should-be (curry #'add-trace! tree)
              run)
  tree)

(defun run->tree (run)
  (let ((tree (make-instance 'standard-node :name "<root>")))
    (add-run! tree run)))
