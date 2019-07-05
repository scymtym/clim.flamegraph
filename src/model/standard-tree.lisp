;;;; standard-tree.lisp --- Default implementations of the tree and node protocols
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.model)

(defclass hit-count-mixin ()
  ((%hit-count :initarg  :hit-count
               :accessor hit-count
               :initform 0)))

(defmethod print-items:print-items append ((object hit-count-mixin))
  `((:hit-count ,(hit-count object) " ~:D hit~:P" ((:after :name)))))

;;; `standard-tree'

(defclass standard-tree ()
  ((%root :initarg  :root
          :reader   root)

   (%names :reader names
           :initform (make-hash-table :test #'equal))
   (%flat :reader   flat
          :initform (make-array 0 :adjustable t :fill-pointer 0))))

;;; `standard-node'

(defclass standard-node (name-mixin
                         hit-count-mixin
                         print-items:print-items-mixin)
  ((%children  :initarg  :children
               :reader   %children
               :initform (make-hash-table :test #'equal)) ; TODO start with list
   ))

(defmethod find-child ((name t) (node standard-node))
  (gethash name (%children node)))

(defmethod (setf find-child) ((new-value t) (name t) (node standard-node))
  (setf (gethash name (%children node)) new-value))

(defmethod ensure-child ((name t) (node standard-node) (thunk function))
  (ensure-gethash name (%children node) (funcall thunk)))

;;;

(defclass flat-node (name-mixin
                     hit-count-mixin
                     print-items:print-items-mixin)
  ())

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
  (let ((node (root tree)))
    (incf (hit-count node))
    (map-samples
     (lambda (sample)
       (unless (labels ((rec (thing)
                          (typecase thing
                            (qualified-name (string= "CLIM.FLAMEGRAPH.BACKEND.ADVICE"
                                                     (container thing)))
                            (string         (or (search "CLIM.FLAMEGRAPH.BACKEND.ADVICE" thing)
                                                (search "RECORDING-CALL" thing)))
                            (cons           (some #'rec thing)))))
                 (rec (name sample)))

         (let ((flat (ensure-gethash (name sample) (names tree)
                                     (let ((node (make-instance 'flat-node :name (name sample))))
                                       (vector-push-extend node (flat tree))
                                       node))))
           (incf (hit-count flat)))

         (let* ((name  (name sample))
                (child (ensure-child name node (lambda ()
                                                 (make-instance 'standard-node
                                                                :name name)))))
           (incf (hit-count child))
           (setf node child))))
     trace)
    tree))

(defmethod add-run! (tree run)
  (map-traces (lambda (trace)
                (when (typep trace 'standard-trace) ; TODO HACK
                  (add-trace! tree trace)))
              #+should-be (curry #'add-trace! tree)
              run)

  (sort (flat tree) #'> :key #'hit-count)

  tree)

(defun run->tree (run)
  (let* ((root (make-instance 'standard-node :name "<root>"))
         (tree (make-instance 'standard-tree :root root)))
    (add-run! tree run)))
