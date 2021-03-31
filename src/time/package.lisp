(cl:defpackage #:clim.flamegraph.time
  (:use
   #:cl)

  (:export
   #:real-time)

  (:export
   #:print-human-readable-duration))

(cl:in-package #:clim.flamegraph.time)

(defun real-time ()
  #+sbcl (multiple-value-bind (seconds microseconds)
             (sb-unix::get-time-of-day)
           (+ (* 1000000 seconds) microseconds))
  #-sbcl (get-internal-real-time))

(defconstant time-units-per-second
  #+sbcl 1000000
  #-sbcl internal-time-units-per-second)
