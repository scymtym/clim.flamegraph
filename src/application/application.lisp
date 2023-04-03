;;;; application.lisp --- Main application frame.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfaak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.application)

;;; `command-button'

(defclass command-button (clim:push-button-pane)
  ((%command :initarg :command
             :reader  command)))

(defmethod clim:activate-callback ((gadget command-button) client gadget-id)
  (clim:execute-frame-command client (command gadget)))

(defmethod clim:note-command-enabled ((frame-manager t)
                                      (frame         clim:application-frame)
                                      (command       t))
  (clim:map-over-sheets (lambda (sheet)
                          (when (and (typep sheet 'command-button)
                                     (eq (ensure-car command)
                                         (ensure-car (command sheet))))
                            (clim:activate-gadget sheet)))
                        (clim:frame-top-level-sheet frame)))

(defmethod clim:note-command-disabled ((frame-manager t)
                                       (frame         clim:application-frame)
                                       (command       t))
  (clim:map-over-sheets (lambda (sheet)
                          (when (and (typep sheet 'command-button)
                                     (eq (ensure-car command)
                                         (ensure-car (command sheet))))
                            (clim:deactivate-gadget sheet)))
                        (clim:frame-top-level-sheet frame)))

;;; Application frame

(defun make-timeline-model (run)
  (let ((start-time   most-positive-fixnum)
        (end-time     most-negative-fixnum)
        (lanes        '())
        (thread->lane (make-hash-table :test #'eq)))

    (setf start-time (model:start-time run)
          end-time   (model:end-time run))
    (flet ((name (thread)
             (cond
               ((bt:threadp thread)
                (princ-to-string (bt:thread-name thread)))
               ((typep thread 'model::standard-thread)
                (model:name thread))
               (t
                (string thread)))))
      (model:map-traces
       (lambda (thing)
         (let* ((thread (when (compute-applicable-methods #'model:thread (list thing))
                          (model:thread thing)))
                (lane   (or (gethash thread thread->lane)
                            (let ((lane (make-instance 'timeline::lane
                                                       :description (name thread))))
                              (push lane lanes)
                              (setf (gethash thread thread->lane) lane)
                              lane))))
           (push thing (timeline::elements lane))))
       run))
    (make-instance 'timeline::timeline
                   :lanes      (if-let ((global (find "GLOBAL" lanes :key #'timeline::description :test #'string=)))
                                 (list* global (remove global lanes))
                                 lanes)
                   :start-time start-time
                   :end-time   end-time)))

(clim:define-application-frame performance-analyzer ()
  (;; Recording
   (%configuration :initarg  :configuration
                   :reader   configuration
                   :initform (make-instance 'source-configuration))
   (%recorder      :accessor recorder
                   :initform nil)
   (%run           :initarg  :run
                   :accessor run)
   ;; Recorded runs
   (%runs          :initarg  :runs
                   :accessor runs
                   :initform '())
   (%focus         :initarg  :focus
                   :accessor focus))
  (:panes
   ;; Recording
   (recording-status  recording-status-pane)
   (threads           thread-list-pane :configuration (configuration clim:*application-frame*))
   (sources           source-configuration-pane :configuration (configuration clim:*application-frame*))
   (start-recording   command-button     :label   "Start Recording"
                                         :command '(com-start-recording)
                                         :active  t
                                         #+no :activate-callback #+no  (lambda (gadget)
                                                                         (declare (ignore gadget))
                                                                         (clim:execute-frame-command
                                                                          clim:*application-frame* '(com-start-recording))))
   (stop-recording    command-button     :label   "Stop Recording"
                                         :command '(com-stop-recording)
                                         :active  nil)
   (suspend-recording clim:toggle-button :label   "Suspend Recording"
                                         :active  nil)
   ;; Run List
   #+no (run-list clim:list-pane :items (runs clim:*application-frame*)
                                 :presentation-type-key (constantly 'run))
   (run-list :application :display-function (lambda (frame pane)
                                              (clim:format-items
                                               (runs frame)
                                               :stream pane
                                               :printer (lambda (item stream)
                                                          (clim:present
                                                           item `(run :selected? ,(eq item (run frame)))
                                                           :stream stream :single-box t))))
                          :default-view     (make-instance 'run-list-view))
   ;;
   (time-scale clim:slider-pane
               :label                  "Zoom"
               :min-value              -3
               :max-value              8
               :value                  1.5
               :orientation            :horizontal
               :show-value-p           t
               :decimal-places         2
                                        ; :drag-callback          #'time-scale-changed-callback

               :value-changed-callback (lambda (gadget value)
                                         (let* ((frame     (clim:gadget-client gadget))
                                                (timeline2 (clim:find-pane-named frame 'timeline))
                                                (timeline (timeline::right timeline2)))
                                           (setf (timeline::scale timeline) (expt 10 value))
                                           (clim:redisplay-frame-pane frame (timeline::left timeline2))
                                           (clim:redisplay-frame-pane frame timeline))))
   (depth-limit  clim:slider-pane
                 :label          "Depth Cutoff"
                 :min-value      1
                 :max-value      100
                 :value          10
                 :orientation    :horizontal
                 :show-value-p   t
                 :decimal-places 0
                 :value-changed-callback
                 (lambda (gadget value)
                   (let* ((frame     (clim:gadget-client gadget))
                          (timeline2 (clim:find-pane-named frame 'timeline))
                          (timeline (timeline::right timeline2)))
                     (setf (timeline::depth-limit timeline) value)
                     (setf (timeline::depth-limit (timeline::left timeline2)) value)
                     (clim:redisplay-frame-pane frame timeline)
                     (clim:redisplay-frame-pane frame (timeline::left timeline2)))))
   #+later (show-threads clim:toggle-button :label "Show Profiler Threads")
   (timeline     (let ((model (make-timeline-model (make-instance 'model::standard-run :traces (list )))))
                   (clim:make-pane 'timeline::timeline2-pane
                                   :left  (clim:make-pane 'timeline::lane-description-pane :model model)
                                   :right (clim:make-pane 'timeline::timeline-pane :max-width clim:+fill+
                                                                                   :model model))))
   (interactor :interactor))
  (:command-table (performance-analyzer :inherit-from (source-command-table
                                                       run-command-table)))
  (:menu-bar nil)
  (:layouts
   (:default
    (clim:vertically ()
      (clim:horizontally ()
        (1/4   (clim:labelling (:label "Runs")
                 run-list))
        (clim:make-pane 'clime:box-adjuster-gadget)
        (:fill (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page)
                 ("Record"
                  (clim:vertically ()
                    (clim:spacing (:thickness 8)
                      (clim:horizontally (:x-spacing 8)
                        (clim:labelling (:label "Threads")
                          threads)
                        (:fill (clim:labelling (:label "Sources")
                                 sources))
                        (clim:vertically (:y-spacing 8)
                          start-recording
                          stop-recording
                          suspend-recording
                          :fill)))
                    (:fill recording-status)))
                 ("Analyze Run"
                  (clim:vertically ()
                    (clim:horizontally (:x-spacing 8)
                      time-scale
                      depth-limit
                      #+later show-threads
                      :fill)
                    (:fill (clim:scrolling (:scoll-bars t)
                             (clim:spacing (:thickness 8)
                               timeline)))))
                 #+no ("Test"
                  (clim:scrolling ()
                    (clim:make-pane 'flat::flat-pane2))))))
      (clim:make-pane 'clime:box-adjuster-gadget)
      (1/16 interactor))))
  (:pointer-documentation t)
  ;; (:update-instances-on-redefinition t)
  )

;;; Recording view

(clim:define-command (com-start-recording :command-table performance-analyzer
                                          :name          t)
    ()
  (let* ((frame         clim:*application-frame*)
         (configuration (find-configuration frame))
         (recorder      (recording:make-recorder :standard :sources (sources configuration)))
         (run           (recording:make-run recorder)))
    (setf (recorder frame) recorder
          (run frame)      run)
    (setf (run (clim:find-pane-named frame 'recording-status)) run)
    (recording:setup recorder run)
    (recording:start recorder run)

    (setf (clim:command-enabled 'com-start-recording frame) nil
          (clim:command-enabled 'com-stop-recording frame)  t)))

(clim:define-command (com-stop-recording :command-table performance-analyzer
                                         :name          t)
    ()
  (let* ((frame    clim:*application-frame*)
         (recorder (recorder frame))
         (run      (run frame)))
    (recording:stop recorder run)
    (recording:teardown recorder run)
    (let ((run (make-instance 'model::standard-run :traces (recording::data run))))
      (com-add-run run))

    (setf (clim:command-enabled 'com-start-recording frame) t
          (clim:command-enabled 'com-stop-recording frame)  nil)))

;;; Timeline view

(defun set-timeline-model (timeline model region)
  (let ((left (timeline::left timeline))
        (right (timeline::right timeline))
        (model (or model (make-timeline-model (make-instance 'model::standard-run :traces (list region))))))
    (setf (timeline::scale right) (/ (- (clim:bounding-rectangle-width
                                         (clim:untransform-region (clim:sheet-native-transformation timeline)
                                                                  (clim:sheet-native-region timeline)))
                                        100
                                        100)
                                     (max .000000001 (model:duration model)))
          (clim:gadget-value (clim:find-pane-named clim:*application-frame* 'time-scale) :invoke-callback nil)
          (log (timeline::scale right) 10))
    (reinitialize-instance left :model model)
    (reinitialize-instance right :model model)
    (clim:redisplay-frame-pane clim:*application-frame* left)
    (clim:redisplay-frame-pane clim:*application-frame* right)))

(macrolet ((define-zoom-command (name operator keystroke)
             `(clim:define-command (,name :command-table performance-analyzer
                                          :name          t
                                          :keystroke     ,keystroke)
                  ()
                (let ((gadget (clim:find-pane-named clim:*application-frame* 'time-scale)))
                  (setf (clim:gadget-value gadget :invoke-callback t)
                        (,operator (clim:gadget-value gadget) .2))))))
  (define-zoom-command com-zoom-in  + (#\+ :control))
  (define-zoom-command com-zoom-out - (#\- :control)))

(clim:define-command (com-focus-region :command-table performance-analyzer
                                       :name          t)
    ((region 'clim.flamegraph.view.region::region :gesture :select))
  (let ((timeline (clim:find-pane-named clim:*application-frame* 'timeline)))
    (set-timeline-model timeline nil region)))

(clim:define-command (com-reset :command-table performance-analyzer
                                :name          t)
    ((area 'clim:blank-area
           :gesture (:select
                     :tester                ((object)
                                             (and object
                                                  (clim:sheet-ancestor-p
                                                   (clim:event-sheet object)
                                                   (clim:find-pane-named
                                                    clim:*application-frame* 'timeline))))
                     :documentation         "Reset zoom"
                     :pointer-documentation "Reset zoom"
                     :echo                  nil)))
  (let* ((timeline (clim:find-pane-named clim:*application-frame* 'timeline))
         (model    (make-timeline-model (run clim:*application-frame*))))
    (set-timeline-model timeline model nil)))

(clim:define-command (com-thread-flamegraph :command-table performance-analyzer
                                            :name          t)
    ((traces '(or timeline::thread timeline::traces)
             :gesture (:select
                       :priority              2
                       :echo                  nil
                       :documentation         "Show flamegraph for traces"
                       :pointer-documentation "Show flamegraph for traces")))
  (let* ((run  (make-instance 'model::standard-run :traces (typecase traces
                                                             (list traces)
                                                             (timeline::lane (timeline::elements traces)))))
         (tree (model::run->tree run)))
    (bt:make-thread
     (lambda ()
       (clim:run-frame-top-level
        (clim:make-application-frame 'flamegraph-frame :run run :tree tree))))))

(clim:define-command (com-run-flamegraph :command-table performance-analyzer
                                         :name          t)
    ((run 'run :gesture :select))
  (let ((tree (model::run->tree run)))
    (bt:make-thread
     (lambda ()
       (clim:run-frame-top-level
        (clim:make-application-frame 'flamegraph-frame :run run :tree tree))))))

(clim:define-command (com-inspect :command-table performance-analyzer
                                  :name          t)
    ((inspectable '(or clim.flamegraph.view.region::inspectable timeline::traces timeline::thread timeline::event view::name run)
                  :gesture (:select
                            :documentation "Inspect thing"
                            :pointer-documentation "Inspect thing")))
  (clouseau:inspect inspectable :new-process t))

(clim:define-command (com-dummy :command-table performance-analyzer
                                :name          t)
    ((event 'timeline::event :gesture :select)))

;;; Test

(clim:define-command (com-highlight-fake :command-table performance-analyzer
                                         :name          t)
    ((thing 'integer :gesture :select)))

(clim:define-command (com-add-run :command-table performance-analyzer)
    ((run t))
  (let ((frame clim:*application-frame*))
    (push run (runs frame))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'run-list))))

;;; Entry point

(defun launch (&key (runs '()) (run nil) new-process)
  (let ((frame (clim:make-application-frame 'performance-analyzer :runs runs
                                                                  :run  run)))
    (flet ((do-it ()
             (clim:run-frame-top-level frame)))
      (if new-process
          (bt:make-thread #'do-it)
          (do-it)))
    frame))

(defun show-run (run &key new-process)
  (launch :run run :new-process new-process))

(defun add-run (frame run)
  (clim:execute-frame-command frame (list 'com-add-run run)))
