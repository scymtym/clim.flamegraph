(cl:in-package #:clim.flamegraph.recording)

(defclass standard-run ()
  ((%queue :reader   %queue
           :initform (lparallel.queue:make-queue))
   (%data  :accessor data
           :initform '())
   #+maybe-later (%run   :accessor run
           :initform (make-instance 'clim.flamegraph.model::standard-run))))

(defclass standard-recorder ()
  ((%sources :initarg  :sources
             :type     list
             :reader   sources)
   (%thread  :accessor %thread)))

(defmethod make-run ((recorder standard-recorder))
  (make-instance 'standard-run))

;;; Lifecycle

(defmethod setup ((recorder standard-recorder) (run standard-run))
  (map nil (rcurry #'setup run) (sources recorder))
  (setf (%thread recorder) (bt:make-thread (curry #'work recorder run)
                                           :name "recording worker"))
  (values))

(defmethod start ((recorder standard-recorder) (run standard-run))
  (map nil (rcurry #'start run) (sources recorder)))

(defmethod stop ((recorder standard-recorder) (run standard-run))
  (map nil (rcurry #'stop run) (sources recorder)))

(defmethod teardown ((recorder standard-recorder) (run standard-run))
  (map nil (rcurry #'teardown run) (sources recorder))
  (lparallel.queue:push-queue :end (%queue run))
  (bt:join-thread (%thread recorder))
  (values))

;;; Recorder

(defmethod work ((recorder standard-recorder) (run t))
  (loop :with queue = (%queue run)
        :for item = (lparallel.queue:pop-queue queue)
        :until (eq item :end)
        :do (handle-chunk recorder run item)))

(defmethod handle-chunk ((recorder standard-recorder)
                         (run      standard-run)
                         (chunk    t))
  (map nil (curry #'handle-item recorder run) chunk))

(defmethod handle-item ((recorder standard-recorder)
                        (run      standard-run)
                        (item     t))
  (push item (data run)))

;;; Source

(defmethod add-chunk ((source t)
                      (run    standard-run)
                      (chunk  t))
  (lparallel.queue:push-queue chunk (%queue run)))
