(cl:in-package #:clim.flamegraph.application)

;;; Configuration

(clim:define-command-table source-command-table)

;;; Configuration node

(clim:define-command (com-remove-node :command-table source-command-table
                                      :name          t)
    ((node 'configuration-node :gesture :delete))
  ;; (removef (sources (find-configuration clim:*application-frame*)) source)

  (when (typep node 'cons)
    (when-let* ((configuration (find-configuration clim:*application-frame*))
                (source        (find-if (of-type 'clim.flamegraph.backend.advice:source)
                                        (sources configuration))))
      (reinitialize-instance source :specification (remove node (clim.flamegraph.backend.advice::specification
                                                                 source)))))

  (when (bt:threadp node)
    (removef *threads* node)

    (when-let* ((configuration (find-configuration clim:*application-frame*))
                (source        (find-if (of-type 'clim.flamegraph.backend.advice:source)
                                        (sources configuration))))
      (reinitialize-instance source :thread-test (let ((threads *threads*))
                                                   (lambda (thread)
                                                     (member thread threads)))))

    (when-let* ((configuration (find-configuration clim:*application-frame*))
                (source        (find-if (of-type 'clim.flamegraph.backend.sb-sprof:source)
                                        (sources configuration))))
      (reinitialize-instance source :thread-test (let ((threads *threads*))
                                                   (lambda (thread)
                                                     (member thread threads)))))))

;;;

(clim:define-presentation-method clim:accept ((type   configured-thread)
                                              (stream t)
                                              (view   t)
                                              &rest args &key)
  (let* ((items (map 'list (lambda (thread)
                             (list (bt:thread-name thread) thread))
                     (set-difference (bt:all-threads) configured-threads)))
         (type  `(clim:completion ,items :value-key second)))
    (apply #'clim:accept type :stream stream :view view :prompt nil args)))

(clim:define-command (com-add-thread :command-table source-command-table
                                     :name          t)
    ((thread `(configured-thread :configured-threads ,*threads*)))
  (push thread *threads*)

  (when-let* ((configuration (find-configuration clim:*application-frame*))
              (source        (find-if (of-type 'clim.flamegraph.backend.advice:source)
                                      (sources configuration))))
    (reinitialize-instance source :thread-test (let ((threads *threads*))
                                                 (lambda (thread)
                                                   (member thread threads)))))

  (when-let* ((configuration (find-configuration clim:*application-frame*))
              (source        (find-if (of-type 'clim.flamegraph.backend.sb-sprof:source)
                                      (sources configuration))))
    (reinitialize-instance source :thread-test (let ((threads *threads*))
                                                 (lambda (thread)
                                                   (member thread threads))))))

(clim:define-presentation-to-command-translator
    non-configured-thread->com-add-thread
    (non-configured-thread com-add-thread source-command-table)
  (object)
  (list object))

;;; Sources

(clim:define-presentation-method clim:accept ((type   source)
                                              (stream t)
                                              (view   t)
                                              &rest args &key)
  (flet ((make-item (source)
           (case source
             (clim.flamegraph.backend.sb-sprof::source
              '("Statistical Profiler" clim.flamegraph.backend.sb-sprof::source))
             (clim.flamegraph.backend.advice:source
              '("Tracer"               clim.flamegraph.backend.advice:source))
             (clim.flamegraph.backend.sb-memory:source
              '("Memory Events"        clim.flamegraph.backend.sb-memory:source)))))
    (let* ((items (map 'list #'make-item
                       (set-difference '(clim.flamegraph.backend.sb-sprof::source
                                         clim.flamegraph.backend.advice:source
                                         clim.flamegraph.backend.sb-memory:source)
                                       configured-sources)))
           (type  `(clim:completion ,items :value-key second)))
      (apply #'clim:accept type :stream stream :view view :prompt nil args))))

(clim:define-command (com-add-source :command-table source-command-table
                                     :name          t)
    ((configuration 'configuration-root
                    :gesture        :select
                    :prompt         "Configuration"
                    :default        (find-configuration clim:*application-frame*)
                    :insert-default t)
     (source        `(source :configured-sources ,(map 'list #'type-of
                                                       (sources (find-configuration clim:*application-frame*))))
                    :prompt "Source kind"))
  (push (case source
          (clim.flamegraph.backend.advice:source (make-instance source :specification '((:blockers t) (:io t))))
          (t                                     (make-instance source)))
        (sources configuration)))

(clim:define-command (com-remove-source :command-table source-command-table
                                        :name          t)
    ((source 'source :gesture :delete))
  (removef (sources (find-configuration clim:*application-frame*)) source))


(clim:define-command (com-add-specification :command-table source-command-table
                                            :name          t)
    ((source        'advice-source
                    :prompt         "Source"
                    :gesture        :select
                    :default        (find-if (of-type 'clim.flamegraph.backend.advice:source)
                                             (sources (find-configuration clim:*application-frame*)))
                    :insert-default t)
     (specification `(or (clim:completion (:blockers :io))
                         (clim:member-alist
                          ,(map 'list (lambda (package)
                                        (list (package-name package) package))
                                (list-all-packages)))
                         symbol)
                    :prompt "Symbol or package to trace")
     &key
     (arguments 'boolean :default nil))
  (reinitialize-instance source :specification (list* (list specification arguments)
                                                      (clim.flamegraph.backend.advice::specification source))))

;;; Runs

(clim:define-command-table run-command-table)

(clim:define-command (com-select-run :command-table run-command-table
                                     :name          t)
    ((run 'run :gesture (:select :priority 2)))
  (let ((frame clim:*application-frame*))
    (setf (run frame)   run
          (focus frame) run)
    (when-let* ((timeline (clim:find-pane-named frame 'timeline))
                (model    (make-timeline-model (run frame))))
               (set-timeline-model timeline model nil))))

(clim:define-command (com-delete-run :command-table run-command-table
                                     :name          t)
    ((run 'run :gesture (:delete
                         :documentation "Delete run")))
  (let ((frame clim:*application-frame*))
    (when (eq (run frame) run)
      (setf (focus frame) nil
            (run frame)   nil))
    (removef (runs frame) run)))

(clim:define-command (com-rename-run :command-table run-command-table
                                     :name          t)
    ((run  'run    :gesture (:edit
                             :documentation "Rename run"))
     (name 'string :default (with-output-to-string (stream)
                              (print-run-time run stream))
                   :insert-default t))
  (setf (model:name run) name))

(clim:define-command (com-load-run :command-table run-command-table
                                   :name          t)
    ((filename pathname #+maybe '(pathname :default-type "cl-store")))
  (let ((frame clim:*application-frame*)
        (run   (uiop:symbol-call '#:cl-store '#:restore filename)))
    (push run (runs frame))))

(clim:define-command (com-save-run :command-table run-command-table
                                   :name          t)
    ((run      'run :gesture (:select
                              :documentation "Save run to file"))
     (filename 'pathname))
  (uiop:symbol-call '#:cl-store '#:store run filename))
