;;;; colors.lisp --- Functions for deriving colors from symbols.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view)

(defun dark? (color)
  (< (clim:color-ihs color) .5))

(defun hash-color (hash &key (style :light) desaturize?)
  (clim:make-ihs-color (case style
                         (:light 1)
                         (:dark  .5))
                       (/ (mod hash 1024) 1024)
                       (* (case style
                            (:light .3)
                            (:dark  .8))
                          (if desaturize? .2 1))))

(defun color-variant (base index &key (style :light))
  (let* ((index  (mod index 10))
         (offset (* .2d0 1/9 index)))
    (multiple-value-bind (intensity hue saturation) (clim:color-ihs base)
      (clim:make-ihs-color (- intensity offset) hue saturation))))

(defun package-color (package &key (style :light))
  (hash-color (sxhash (package-name package))
              :style       style
              :desaturize? (or (eq package (find-package '#:common-lisp))
                               (starts-with-subseq "SB-" (package-name package)))))

(defmethod symbol-color ((symbol symbol) &key (style :light))
  (color-variant (if-let ((package (symbol-package symbol)))
                   (package-color package :style style)
                   (hash-color (sxhash nil) :style style))
                 (sxhash (symbol-name symbol))
                 :style style))

(defmethod symbol-color ((symbol string) &key (style :light))
  (let ((base (hash-color 341241 :style style)))
    (color-variant base (sxhash symbol) :style style)))

(defmethod symbol-color ((symbol t) &key (style :light))
  (let ((base (hash-color 54542 :style style)))
    (color-variant base (sxhash symbol) :style style)))

(defun symbol-from-list-name (list)
  ;; Attempt to get something useful for LAMBDA, FLET ... :in ...,
  ;; etc.
  (typecase list
    #+sbcl ((cons (eql lambda) (cons t (cons (eql :in) (cons symbol))))
            (fourth list))
    #+sbcl ((cons (member flet labels) (cons t (cons (eql :in) (cons symbol null))))
            (fourth list))
    #+sbcl ((cons (member flet labels) (cons symbol t))
            (second list))
    #+sbcl ((cons (eql sb-pcl::fast-method) (cons symbol t))
            (second list))
    (t
     (or (second list) (first list)))))

(defmethod symbol-color ((symbol cons) &key (style :light))
  (symbol-color (symbol-from-list-name symbol) :style style))

(defmethod symbol-color ((symbol model::qualified-name) &key (style :light))
  (let ((base (hash-color (sxhash (model::container symbol)) :style style)))
    (color-variant base (sxhash (model:name symbol)) :style style)))
