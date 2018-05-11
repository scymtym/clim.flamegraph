;;;; colors.lisp --- Functions for deriving colors from symbols.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph)

(defun hash-color (hash)
  (flet ((component (value)
           (+ .1d0 (* .5d0 (/ value 255)))))
    (clim:make-rgb-color (component (ldb (byte 8  0) hash))
                         (component (ldb (byte 8  8) hash))
                         (component (ldb (byte 8 16) hash)))))

(defun color-variant (base index)
  (let ((index (mod index 10)))
    (multiple-value-bind (red green blue) (clim:color-rgb base)
      (clim:make-rgb-color (+ red   (* .4d0 1/9 index))
                           (+ green (* .4d0 1/9 index))
                           (+ blue  (* .4d0 1/9 index))))))

(defun package-color (package)
  (hash-color (sxhash (package-name package))))

(defmethod symbol-color ((symbol symbol))
  (color-variant (if-let ((package (symbol-package symbol)))
                   (package-color package)
                   (hash-color (sxhash nil)))
                 (sxhash (symbol-name symbol))))

(defmethod symbol-color ((symbol string))
  (color-variant (hash-color 341241) (sxhash symbol)))

(defmethod symbol-color ((symbol t))
  (color-variant (hash-color 54542) (sxhash symbol)))

(defmethod symbol-color ((symbol cons))
  (typecase symbol
    ;; Special case.
    #+no-longer-needed? ((cons (member sb-impl::optimized-data-vector-ref
                                       sb-impl::optimized-data-vector-set))
                         (symbol-color (first symbol)))
    ;; Attempt to get something useful for LAMBDA.
    ((cons (eql lambda) (cons t (cons symbol #+later (eql in) (cons symbol))))
     (symbol-color (fourth symbol)))
    (t
     (symbol-color (or (second symbol) (first symbol))))))
