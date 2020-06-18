(cl:in-package #:clim.flamegraph.backend.sb-memory)

(defun make-event (name time used)
  (make-instance 'model::standard-event
                 :name       name
                 :time       time
                 :properties (list :used used)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct gc-info
    (start-time 0 :type (unsigned-byte 64))
    (used       0 :type (and fixnum (integer 0)))))

(sb-ext:defglobal **gc-start** (make-gc-info))

(defun note-gc-start ()
  (let ((info **gc-start**))
    (setf (gc-info-start-time info) (time:real-time)
          (gc-info-used       info) (sb-vm::dynamic-usage))))

(defun note-gc-end (source run)
  (clim.flamegraph.recording:add-chunk
   source run (list (let ((info **gc-start**))
                      (make-event :gc-start
                                  (gc-info-start-time info)
                                  (gc-info-used info)))
                    (make-event :gc-end
                                (time:real-time)
                                (sb-vm::dynamic-usage)))))

(defun instrumented-sub-gc (source run function &rest args)
  (declare (type function function)
           (dynamic-extent args))
  (note-gc-start)
  (prog1
      (apply function args)
    (note-gc-end source run)))
