;;;; colors.lisp --- Functions for deriving colors from symbols.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.view)

(defun hash-color (hash &key (style :light))
  (clim:make-ihs-color (case style
                         (:light 1)
                         (:dark  .5))
                       (/ (mod hash 1024) 1024)
                       (case style
                         (:light .3)
                         (:dark  .8)))
  #+no (flet ((component (value)
           (ecase style
             (:light (+ .3d0 (* .3d0 (/ value 255))))
             (:dark  (+ .1d0 (* .3d0 (/ value 255)))))))

    (clim:make-rgb-color (component (ldb (byte 8  0) hash))
                         (component (ldb (byte 8  8) hash))
                         (component (ldb (byte 8 16) hash)))))

(defun color-variant (base index &key (style :light))
  (let* ((index  (mod index 10))
         (offset (* .2d0 1/9 index)))
    (multiple-value-bind (intensity hue saturation) (clim:color-ihs base)
      (clim:make-ihs-color (- intensity offset)
                           hue
                           saturation)))
  #+no (let* ((index  (mod index 10))
              (offset (* .3d0 1/9 index)))
         (multiple-value-bind (red green blue) (clim:color-rgb base)
           (clim:make-rgb-color (+ red   offset)
                                (+ green offset)
                                (+ blue  offset)))))

(defun package-color (package &key (style :light))
  (hash-color (sxhash (package-name package)) :style style))

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

(defmethod symbol-color ((symbol cons) &key (style :light))
  (typecase symbol
    ;; Attempt to get something useful for LAMBDA.
    ((cons (eql lambda) (cons t (cons symbol #+later (eql in) (cons symbol))))
     (symbol-color (fourth symbol) :style style))
    (t
     (symbol-color (or (second symbol) (first symbol)) :style style))))

(defmethod symbol-color ((symbol model::qualified-name) &key (style :light))
  (let ((base (hash-color (sxhash (model::container symbol)) :style style)))
    (color-variant base (sxhash (model:name symbol)) :style style)))
