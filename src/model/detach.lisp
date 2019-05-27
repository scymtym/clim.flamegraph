(cl:in-package #:clim.flamegraph.model)

(defun detach-name (name seen)
  (typecase name
    (symbol (ensure-gethash
             name seen
             (make-instance 'qualified-name
                            :container (package-name (symbol-package name))
                            :name      (symbol-name name))))
    (t (ensure-gethash
        name seen
        (princ-to-string name)))))

(defun detach-region! (node seen)
  (labels ((rec (node)
             (when (typep node 'root-region-mixin)
               (reinitialize-instance node :thread (detach-thread (thread node) seen)))
             (reinitialize-instance node :name (detach-name (name node) seen))
             (when (compute-applicable-methods #'values* (list node))
               (reinitialize-instance node :values (map 'list (lambda (v)
                                                                (ensure-gethash
                                                                 v seen
                                                                 (let ((*print-level* 3) (*print-length* 5))
                                                                   (princ-to-string v))))
                                                        (values* node))))
             (when (compute-applicable-methods #'lock (list node))
               (reinitialize-instance node :lock (let ((lock (lock node)))
                                                   (ensure-gethash
                                                    lock seen
                                                    (let ((*print-level* 3) (*print-length* 5))
                                                      (princ-to-string lock))))))
             (map nil #'rec (children node))))
    (rec node)))

(defun detach-sample! (sample seen)
  (reinitialize-instance sample :name (detach-name (name sample) seen)))

(defun detach-event! (event seen)
  event #+no (reinitialize-instance event :thread (detach-thread (thread event) seen)))

(defun detach-trace! (trace seen)
  (typecase trace
    (standard-region (detach-region! trace seen))
    (standard-trace  (progn
                       (reinitialize-instance trace :thread (detach-thread (thread trace) seen))
                       (map-samples (rcurry #'detach-sample! seen) trace)))
    (standard-event  (detach-event! trace seen))))

(defun detach-thread (thread seen)
  (let ((name (typecase thread
                (sb-thread:thread (sb-thread:thread-name thread))
                (t                thread)))
        (key  (typecase thread
                (sb-thread:thread thread)
                (t                thread))))
    (ensure-gethash key seen (make-instance 'standard-thread :name name))))

(defun detach-run! (run)
  (let ((seen (make-hash-table :test #'equal)))
    (reinitialize-instance
     run :threads (map 'vector (rcurry #'detach-thread seen) (threads run)))
    (map-traces (rcurry #'detach-trace! seen) run)
    run))

#+no (defun detach-recording-state! (recording-state)
  (let ((result (make-hash-table :test #'eq))
        (seen   (make-hash-table :test #'eq)))
    (maphash (lambda (thread state)
               (map nil (rcurry #'detach-node! seen) (thread-state-roots state))
               (setf (gethash (princ-to-string thread) result) state))
             recording-state)

    result))

#+no (defun detach! (recording-state)
  (sb-ext:gc :full t)
  (prog1
      (detach-recording-state! recording-state)
    (sb-ext:gc :full t)))
