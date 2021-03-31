;;;; human-readable.lisp --- Determining and printing orders of magnitude.
;;;;
;;;; Copyright (C) 2015, 2019, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:clim.flamegraph.time)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((expand (spec)
           (destructuring-bind (exponent long short) spec
             (let ((factor       (expt 10 exponent))
                   (long-format  (concatenate 'string "~@[ ~*~]" long  "~@[~A~P~]"))
                   (short-format (concatenate 'string "~@[ ~*~]" short "~@[~A~]")))
               (list factor long-format short-format)))))

    (alexandria:define-constant +non-positive-orders-of-magnitude+
        (map 'list #'expand '((-15 "femto" "f")
                              (-12 "pico"  "p")
                              ( -9 "nano"  "n")
                              ( -6 "micro" "µ")
                              ( -3 "milli" "m")
                              (  0 ""      " ")))
      :test #'equal)

    (alexandria:define-constant +non-negative-orders-of-magnitude+
        (map 'list #'expand '((  3 "kilo"  "K")
                              (  6 "mega"  "M")
                              (  9 "giga"  "G")
                              ( 12 "tera"  "T")
                              ( 15 "exo"   "E")))
      :test #'equal)))

(alexandria:define-constant +orders-of-magnitude+
    (append +non-positive-orders-of-magnitude+
            +non-negative-orders-of-magnitude+)
  :test #'equal)

(alexandria:define-constant +duration-orders-of-magnitude+
    (append +non-positive-orders-of-magnitude+
            `((60                "~@[ ~*~]~*minute~P" "~* m~2*")
              (,(* 60 60)        "~@[ ~*~]~*hour~P"   "~* h~2*")
              (,(* 24 60 60)     "~@[ ~*~]~*day~P"    "~* d~2*")
              ;; Months are uncommon for this
              ;; (,(* 30 24 60 60)  "~@[ ~*~]~*month~P"  "~@[ ~*~]M ~2*")
              (,(* 365 24 60 60) "~@[ ~*~]~*year~P"   "~* y~2*")))
  :test #'equal)

(defun human-readable-value-and-suffix (value table)
  ;; Test dividing VALUE by successively larger factors in TABLE until
  ;; the result is in the desirable range or TABLE runs out.
  (loop :with value = (rationalize value)
        :with zero? = (zerop value)
        :for ((factor1 long short) (factor2)) :on table
        :for (quotient remainder) = (multiple-value-list
                                     (round value factor1))
        ;; If the quotient is in the desirable range, stop the
        ;; search and return.
        :do (cond ;; Special-case the value 0.
              ((and zero? (= 1 factor1))
               (return (values t 0 short long)))
              ;; QUOTIENT is in the desirable range [0, 9] => return.
              ((and (not zero?) (<= 0 quotient 9))
               (return (values (zerop remainder) (/ value factor1) short long)))
              ;; If QUOTIENT is in the desirable range
              ;; ]9, <smallest value that would result in >= 1 w.r.t. FACTOR2>[
              ;; => return. If not, check the other reasons for
              ;; returning and maybe return.
              ((or (not factor2)   ; this is the last available factor
                   (and (not zero?) (zerop quotient)) ; VALUE is too small for available factors
                   (< 9 quotient (/ factor2 factor1))) ; < 1 for w.r.t next factor
               (return (values t quotient short long))))))

(defun print-human-readable-value (stream value
                                   &key
                                   (orders-of-magnitude +orders-of-magnitude+)
                                   signum? space? unit long?)
  (if (realp value)
      (multiple-value-bind (integer? value1 short-format long-format)
          (human-readable-value-and-suffix
           (abs value) orders-of-magnitude)
        (let ((signum (when signum? (signum value))))
          (format stream "~[~;-~; ~;+~]~:[~,1F~;~3D~]~?"
                  (if signum (+ (floor signum) 2) 0) integer? value1
                  (if long? long-format short-format)
                  (list space? unit value1))))
      (let ((width (+ (if signum? 1 0) 3 1 (length unit))))
        (format stream "~V@A" width value))))

(macrolet
    ((define-print-function (suffix &key table unit)
       (let ((name (alexandria:symbolicate 'print-human-readable- suffix)))
         `(defun ,name (stream value &optional colon? at?)
            (let (,@(when unit `((unit (if at? ,@unit)))))
              (print-human-readable-value
               stream value
               ,@(when table `(:orders-of-magnitude ,table))
               ,@(when unit `(:unit unit))
               :signum? colon? :space? at? :long? at?))))))

  (define-print-function order-of-magnitude)
  (define-print-function count :table +non-negative-orders-of-magnitude+)
  (define-print-function size
    :table +non-negative-orders-of-magnitude+
    :unit  ("Byte" "B"))
  (define-print-function duration
    :table +duration-orders-of-magnitude+
    :unit  ("second" "s")))
